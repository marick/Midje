(ns ^{:doc "Environmental factors."}
  midje.util.ecosystem
  (:use [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]])
  (:require [clojure.string :as str]
            midje.util.backwards-compatible-utils))

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
)
