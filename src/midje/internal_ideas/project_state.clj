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

 ;; tools.ns keys are annoyingly long. Shorthand.
 (def unload-key :clojure.tools.namespace.track/unload)
 (def load-key :clojure.tools.namespace.track/load)
 (def filemap-key :clojure.tools.namespace.file/filemap)
 (def deps-key :clojure.tools.namespace.track/deps)
 (def time-key :clojure.tools.namespace.dir/time)

 ;; Global state.

 (defonce state-tracker (atom (assoc (nstrack/tracker)
                                     :desired-directories (directories))))

 ;; What happens when the clock ticks.
 (defn reflect-state-changes [state-tracker project-scanner]
   (apply project-scanner state-tracker (:desired-directories state-tracker)))
 
 (defn file-modification-time [file]
   (.lastModified file))

 ;; What happens in preparation for next tick.
 (defn prepare-for-next-check [state-tracker]
   (assoc state-tracker time-key (latest-modification-time state-tracker)
                        unload-key []
                        load-key []))


 ;; This is what happens between the above.

 (defn show-failure [the-ns throwable]
   (println "LOAD FAILURE for" (ns-name the-ns))
   (println (.getMessage throwable))
   (when (ecosystem/running-in-repl?)
     (println "The exception has been stored in #'*e, so `pst` will show the stack trace.")
     (if (thread-bound? #'*e)
       (set! *e throwable)
       (alter-var-root #'clojure.core/*e (constantly throwable)))))

 (defn shorten-ns-list-by-trying-first [[the-ns & remainder] dependents-cleaner]
   (let [result (try (require the-ns :reload) (catch Throwable t t))]
     (if (isa? (class result) Throwable)
       (do (show-failure the-ns result)
           (dependents-cleaner the-ns remainder))
       remainder)))

 (defn load-namespace-list! [namespaces dependents-cleaner]
   (loop [namespaces namespaces]
     (when (not (empty? namespaces))
       (recur (shorten-ns-list-by-trying-first namespaces
                                               dependents-cleaner)))))

 (defn make-dependents-cleaner [state-tracker]
   (fn [namespace possible-dependents]
     (let [actual-dependents (set (get-in state-tracker [deps-key :dependents namespace]))]
       (remove actual-dependents possible-dependents))))

 (defn latest-modification-time [state-tracker]
   (let [ns-to-file (invert (filemap-key state-tracker))
         relevant-files (map ns-to-file (load-key state-tracker))]
     (apply max (time-key state-tracker)
            (map file-modification-time relevant-files))))


 (defn load-affected-files! [state-tracker]
   (load-namespace-list! (load-key state-tracker)
                         (make-dependents-cleaner state-tracker))
   state-tracker)
 
 (defn loader-for-affected-files-found-by [project-scanner]
   (fn []
     (swap! state-tracker #(-<> %
                                (reflect-state-changes <> project-scanner)
                                load-affected-files!
                                prepare-for-next-check))))

 (def load-everything (loader-for-affected-files-found-by nsdir/scan-all))
 (def load-changed (loader-for-affected-files-found-by nsdir/scan))

)
