(ns ^{:doc "Environmental factors."}
  midje.util.ecosystem
  (:use [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]])
  (:require [clojure.string :as str]
            midje.util.backwards-compatible-utils)
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit]))


(def issues-url "https://github.com/marick/Midje/issues")

(defn clojure-1-3? []
  (and (= 1 (:major *clojure-version*))
       (= 3 (:minor *clojure-version*))))

(defn clojure-1-2-X? []
  (and (= 1 (:major *clojure-version*))
       (= 2 (:minor *clojure-version*))))

(defn clojure-1-2-0? []
  (and (= 1 (:major *clojure-version*))
       (= 2 (:minor *clojure-version*))
       (= 0 (:incremental *clojure-version*))))

(defmacro when-1-3+ [& body] 
  (when-not (= 2 (:minor *clojure-version*))
    `(do ~@body)))

(defmacro unless-1-2-0
  "Skip body completely - including 'Unable to resolve classname' errors."
  [& body]
  (when-not (clojure-1-2-0?)
    `(do ~@body)))

;; The following works because in 1.2 it's parsed as [+  '1].

(def +M (first [+' 1]))
(def -M (first [-' 1]))
(def *M (first [*' 1]))

;;

(defn getenv [var] 
  (System/getenv var))

(defn on-windows? []
  (re-find #"[Ww]in" (System/getProperty "os.name")))

(def line-separator (System/getProperty "line.separator"))



(defn fact-namespaces
  "Return the symbols (suitable for `require`) of namespaces
   that match the args."
  [& args]
  ;; You get an obscure error if you pass a keyword to
  ;; namespaces-in-dir. I'd rather accept all kinds of typos than
  ;; subject a user to that.
  (let [[dirs [_keyword_ prefix & junk]] (split-with string? args)
        desireds (if (empty? dirs) ["test"] dirs) 
        actuals (mapcat namespaces-in-dir desireds)]
    (filter #(.startsWith (name %) (or prefix "")) actuals)))


(defn running-in-repl? []
  (try
    (throw (Exception.))
  (catch Exception e
    (not (empty? (filter #(.contains % "clojure.main$repl$read_eval_print")
                         (map str (.getStackTrace e))))))))

(def home-config-file-name (str/join java.io.File/separator
                                     [(getenv "HOME") ".midje.clj"]))
(def project-config-file-name ".midje.clj")

(defn- file-exists? [name]
  (.isFile (new java.io.File name)))

(defn has-home-config-file? []
  (and (getenv "HOME") (file-exists? home-config-file-name)))

(defn has-project-config-file? []
  (file-exists? project-config-file-name))


;;; Subprocesses 

(def scheduled-futures (atom {}))

(defn schedule [service-tag function interval]
  (let [executor (ScheduledThreadPoolExecutor. 1)
        future (.scheduleWithFixedDelay executor function 0 interval TimeUnit/MILLISECONDS)]
    (swap! scheduled-futures assoc service-tag future)))

(defn stop [service-tag]
  (.cancel (service-tag @scheduled-futures) true)
  (swap! scheduled-futures dissoc service-tag))

;;; Working with project trees

(when-1-3+
  (require '[leiningen.core.project :as project])

  ;; When referring to namespaces on disk, the user intends
  ;; a swath of namespaces. These functions find them.
  (defn project-directories []
    (try
      (let [project (project/read)]
        (concat (:test-paths project) (:source-paths project)))
      (catch java.io.FileNotFoundException e
        ["test"])))
  
  (defn project-namespaces []
    (mapcat namespaces-in-dir (project-directories)))
  
  (defn unglob-partial-namespaces [namespaces]
    (mapcat #(if (= \* (last %))
               (namespaces-on-classpath :prefix (apply str (butlast %)))
               [(symbol %)])
            (map str namespaces)))


  (require '[clojure.tools.namespace.repl :as nsrepl])
  (require '[clojure.tools.namespace.dir :as nsdir])
  (require '[clojure.tools.namespace.track :as nstrack])
  (require '[clojure.tools.namespace.reload :as nsreload])

  (defonce watched-directories (project-directories))
  (defonce nstracker (atom (nstrack/tracker)))
  (def unload-key :clojure.tools.namespace.track/unload)
  (def load-key :clojure.tools.namespace.track/load)
  (def filemap-key :clojure.tools.namespace.file/filemap)
  (def time-key :clojure.tools.namespace.dir/time)
  
  (defn file-modification-time [file]
    (.lastModified file))

  (defn latest-modification-time []
    (apply max (map file-modification-time (keys (filemap-key @nstracker)))))

  (defn novel-namespaces [namespace-finder]
    (swap! nstracker #(apply namespace-finder % watched-directories))
    (load-key @nstracker))

  (defn invert [map]
    (reduce (fn [so-far [key val]]
              (assoc so-far val key))
            {}
            map))

 (defn do-requires [[next-ns & remainder]]
     (when next-ns
       (let [result (try (require next-ns :reload)
                         (catch Throwable t t))]
         (if (isa? (class result) Throwable)
           (do 
             (println "PROGRAM ERROR in" next-ns)
             (println (.getMessage result))
             (when (running-in-repl?)
               (println "The exception has been stored in #'*e, so `pst` will show the stack trace.")
               (if (thread-bound? #'*e)
                 (set! *e result)
                 (alter-var-root #'clojure.core/*e (constantly result))))
             (println "SHOULD HAVE STRIPPED OUT DEPENDENCIES"))
           (recur remainder)))))
    
  (defn require-novelty [namespace-finder]
    (let [work-list (novel-namespaces namespace-finder)
          timestamp (System/currentTimeMillis)]
      (when (not (empty? work-list))
        (println "\n==========")
        (println '== 'Reloading work-list)
        (do-requires work-list)
        ;; There are cases in which default behavior of tools.namespace
        ;; sets the timestamp such that failing files will keep being
        ;; reloaded. So we use our own interpretation.
        (swap! nstracker assoc
               load-key []
               unload-key []  ; This is mainly for tidiness, since we never unload
               time-key timestamp))))
        

  (defn load-everything []
    (require-novelty nsdir/scan-all))
  
  (defn load-changed [& args]
    (require-novelty nsdir/scan))
)
