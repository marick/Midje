(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.internal-ideas.project-state
  (:use [midje.util.form-utils :only [invert]]
        [swiss-arrows.core :only [-<>]]
        [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]])
  (:require [midje.util.ecosystem :as ecosystem]
            midje.util.backwards-compatible-utils))

(ecosystem/when-1-3+

 (require '[clojure.tools.namespace.repl :as nsrepl]
          '[clojure.tools.namespace.dir :as nsdir]
          '[clojure.tools.namespace.track :as nstrack]
          '[clojure.tools.namespace.reload :as nsreload]
          '[leiningen.core.project :as project])


;;; Querying the project tree

 (defn directories []
   (try
     (let [project (project/read)]
       (concat (:test-paths project) (:source-paths project)))
     (catch java.io.FileNotFoundException e
       ["test"])))

 (defn namespaces []
   (mapcat namespaces-in-dir (directories)))

 (defn unglob-partial-namespaces [namespaces]
   (mapcat #(if (= \* (last %))
              (namespaces-on-classpath :prefix (apply str (butlast %)))
              [(symbol %)])
           (map str namespaces)))


;;; Responding to changed files


 (defonce watched-directories (directories))
 (defonce nstracker (atom (nstrack/tracker)))

 (def unload-key :clojure.tools.namespace.track/unload)
 (def load-key :clojure.tools.namespace.track/load)
 (def filemap-key :clojure.tools.namespace.file/filemap)
 (def deps-key :clojure.tools.namespace.track/deps)
 (def time-key :clojure.tools.namespace.dir/time)
 (def next-time-key :next-time)

 (defn file-modification-time [file]
   (.lastModified file))

 (defn latest-modification-time [nstracker]
   (let [ns-to-file (invert (filemap-key nstracker))
         relevant-files (map ns-to-file (load-key nstracker))]
     (apply max (time-key nstracker)
            (map file-modification-time relevant-files))))


 (defn autotest-augment-tracker [nstracker]
   (assoc nstracker next-time-key (latest-modification-time nstracker)))

 (defn autotest-next-tracker [nstracker]
   (assoc nstracker
          time-key (next-time-key nstracker)
          unload-key []
          load-key []))

 (defn first-to-load [nstracker]
   (first (load-key nstracker)))

 (defn without-first-to-load [nstracker]
   (assoc nstracker load-key (rest (load-key nstracker))))

 (defn without-first-to-load-and-dependents [nstracker]
   (let [[first & remainder] (load-key nstracker)
         first-dependents (get-in nstracker [deps-key :dependents first] #{})
         surviving-remainder (remove first-dependents remainder)]
     (assoc nstracker load-key surviving-remainder)))


 (defn work-free-tracker? [tracker]
   (empty? (load-key tracker)))


 (defn show-failure [the-ns throwable]
   (println "LOAD FAILURE for" (ns-name the-ns))
   (println (.getMessage throwable))
   (when (ecosystem/running-in-repl?)
     (println "The exception has been stored in #'*e, so `pst` will show the stack trace.")
     (if (thread-bound? #'*e)
       (set! *e throwable)
       (alter-var-root #'clojure.core/*e (constantly throwable)))))


 (defn load-one [nstracker]
   (let [the-ns (first-to-load nstracker)
         result (try (require the-ns :reload)
                     (catch Throwable t t))]
     (if (isa? (class result) Throwable)
       (do (show-failure the-ns result)
           (without-first-to-load-and-dependents nstracker))
       (without-first-to-load nstracker))))

 (defn obey-tracker [nstracker]
   (if (work-free-tracker? nstracker)
     nstracker
     (recur (load-one nstracker))))

 (defn react-to-any-changed-files! [namespace-finder]
   (swap! nstracker #(-<> %
                          (apply namespace-finder <> watched-directories)
                          autotest-augment-tracker
                          obey-tracker
                          autotest-next-tracker)))

 (defn load-everything []
   (react-to-any-changed-files! nsdir/scan-all))

 (defn load-changed [& args]
   (react-to-any-changed-files! nsdir/scan))

)
