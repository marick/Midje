(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.internal-ideas.project-state
  (:use [midje.util.form-utils :only [invert separate-by]]
        [swiss-arrows.core :only [-<>]]
        [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]])
  (:require [midje.emission.boundaries :as emission-boundary]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.colorize :as color]
            [midje.config :as config]
            [midje.clojure-test-facade :as ctf]
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
       ;; Note that order is important here; see below.
       (concat (:test-paths project) (:source-paths project)))
     (catch java.io.FileNotFoundException e
       ["test"])))

 (defn namespaces []
   (mapcat namespaces-in-dir (directories)))

 ;; For some purposes, it matters that the :tests files
 ;; come before the :sources files. That happens to always be
 ;; true, but the names below emphasize it.
 (def directories-test-first directories)
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
 (def respond-to-require-failure (fn [& args]))

 (defmacro on-require-failure [args & forms]
   `(alter-var-root ~#'respond-to-require-failure
                    (constantly (fn ~args ~@forms))))

 (defn file-modification-time [file]
   (.lastModified file))

 (defn latest-modification-time [state-tracker]
   (let [ns-to-file (invert (filemap-key state-tracker))
         relevant-files (map ns-to-file (load-key state-tracker))]
     (apply max (time-key state-tracker)
            (map file-modification-time relevant-files))))


 (defn require-namespaces! [namespaces clean-dependents]
   (letfn [(broken-source-file? [the-ns]
             (try
               (require the-ns :reload)
               false
             (catch Throwable t
               (respond-to-require-failure the-ns t)
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
               [new-roots unkilled-descendents] (separate-by actual-dependent-set surviving-namespaces)]
           (recur (concat roots-to-handle-later new-roots)
                  unkilled-descendents))))))
         
 (defn react-to-tracker! [state-tracker]
   (let [namespaces (load-key state-tracker)]
     (when (not (empty? namespaces))
       (emission-boundary/around-namespace-stream namespaces config/no-overrides
         (println (color/note "\n======================================================================"))
         (println (color/note "Loading " (pr-str namespaces)))
         (require-namespaces! namespaces
                              (mkfn:clean-dependents state-tracker))))))

 (defn prepare-for-next-scan [state-tracker]
   (assoc state-tracker time-key (latest-modification-time state-tracker)
                        unload-key []
                        load-key []))

 (defn mkfn:scan-and-react [dirs scanner]
   (fn []
     (swap! state-tracker
            #(let [new-tracker (apply scanner % dirs)]
               (react-to-tracker! new-tracker)
               (prepare-for-next-scan new-tracker)))))


 (defn mkfn:react-to-changes [dirs]
   (mkfn:scan-and-react dirs nsdir/scan))

 (defn load-everything [dirs]
   ((mkfn:scan-and-react dirs nsdir/scan-all)))

)
