(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.data.project-state
  (:use midje.clojure.core
        [swiss-arrows.core :only [-<>]]
        [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]])
  (:require [midje.emission.boundaries :as emission-boundary]
            [midje.util.ecosystem :as ecosystem]
            [midje.emission.colorize :as color]
            [midje.config :as config]))

(ecosystem/when-1-3+

 (require '[clojure.tools.namespace.repl :as nsrepl]
          '[clojure.tools.namespace.dir :as nsdir]
          '[clojure.tools.namespace.track :as nstrack]
          '[clojure.tools.namespace.reload :as nsreload])


;;; Querying the project tree

 (defn namespaces []
   (mapcat namespaces-in-dir (ecosystem/leiningen-paths)))

 ;; For some purposes, it matters that the :test-paths files come
 ;; before the :source-paths files. That happens to always be true,
 ;; but the name below emphasizes it.
 (def namespaces-test-first namespaces)

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

 (defonce state-tracker (atom (nstrack/tracker)))

 (defn file-modification-time [file]
   (.lastModified file))

 (defn latest-modification-time [state-tracker]
   (let [ns-to-file (invert (filemap-key state-tracker))
         relevant-files (map ns-to-file (load-key state-tracker))]
     (apply max (time-key state-tracker)
            (map file-modification-time relevant-files))))


 (defn require-namespaces! [namespaces on-require-failure clean-dependents]
   (letfn [(broken-source-file? [the-ns]
             (try
               (require the-ns :reload)
               false
             (catch Throwable t
               (on-require-failure the-ns t)
               true)))

           (shorten-ns-list-by-trying-first [[the-ns & remainder]]
             (if (broken-source-file? the-ns)
               (clean-dependents the-ns remainder)
               remainder))]

   (loop [namespaces namespaces]
     (when (not (empty? namespaces))
       (recur (shorten-ns-list-by-trying-first namespaces))))))

 ;; TODO: clojure.tools.namespace also finds a transitive closure when it finds
 ;; the namespaces to reload, but I don't see quite how to hook into that mechanism,
 ;; so I roll my own.
 (defn mkfn:clean-dependents [state-tracker]
   (fn [failing-namespace other-namespaces]
     (loop [[root-to-handle & roots-to-handle-later] [failing-namespace]
            surviving-namespaces other-namespaces]
       (if (nil? root-to-handle)
         surviving-namespaces
         (let [actual-dependent-set (set (get-in state-tracker [deps-key :dependents root-to-handle]))
               [new-roots unkilled-descendents] (separate actual-dependent-set surviving-namespaces)]
           (recur (concat roots-to-handle-later new-roots)
                  unkilled-descendents))))))
         
 (defn react-to-tracker! [state-tracker options]
   (let [namespaces (load-key state-tracker)]
     (when (not (empty? namespaces))
       ( (:namespace-stream-checker options)
         namespaces
         #(require-namespaces! namespaces
                               (:on-require-failure options)
                               (mkfn:clean-dependents state-tracker))))))

 (defn prepare-for-next-scan [state-tracker]
   (assoc state-tracker time-key (latest-modification-time state-tracker)
                        unload-key []
                        load-key []))

 (defn mkfn:scan-and-react [options scanner]
   (fn []
     (swap! state-tracker
            #(let [new-tracker (apply scanner % (:files options))]
               (react-to-tracker! new-tracker options)
               (prepare-for-next-scan new-tracker)))))


 (defn mkfn:react-to-changes [options]
   (mkfn:scan-and-react options nsdir/scan))

 (defn load-everything [options]
   ((mkfn:scan-and-react options nsdir/scan-all)))

)
