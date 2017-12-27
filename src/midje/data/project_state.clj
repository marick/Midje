(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.data.project-state
  (:require [clj-time.local :as time]
            [clojure.java.io :as io]
            [clojure.set]
            [midje.config :as config]
            [midje.emission.api :as emit]
            [midje.emission.boundaries :as emission-boundary]
            [midje.emission.colorize :as color]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.bultitude :as tude]
            [such.maps :as map]
            [such.sequences :as seq]))


(require '[clojure.tools.namespace.repl :as nsrepl]
         '[clojure.tools.namespace.dir :as nsdir]
         '[clojure.tools.namespace.track :as nstrack]
         '[clojure.tools.namespace.reload :as nsreload])


;;; Querying the project tree

(defn valid-namespace-symbols [classifieds]
  (letfn [(filenames [fileseq]
            (prn (:file (first fileseq)))
            (prn (.getName ^java.io.File (:file (first fileseq))))
            (into [] (map #(.getName ^java.io.File (:file %)) fileseq)))]
    (let [grouped-by-status (group-by :status classifieds)]
      (when-not (empty? (:unreadable grouped-by-status))
        (prn (:unreadable grouped-by-status))
        (println (color/note "Warning: These files were unreadable: "
                             (filenames (:unreadable grouped-by-status)))))
      (when-not (empty? (:invalid-clojure-file grouped-by-status))
        (println (color/fail "FAILURE: These files have broken namespaces and will not be loaded."))
        (println (color/fail (filenames (:invalid-clojure-file  grouped-by-status))))
        (emit/fail-silently))
      (map :namespace-symbol (:contains-namespace grouped-by-status)))))

(defn namespaces []
  (valid-namespace-symbols (mapcat tude/classify-dir-entries (ecosystem/leiningen-paths))))
;; For some purposes, it matters that the :test-paths files come
;; before the :source-paths files. That happens to always be true,
;; but the name below emphasizes it.
(def namespaces-test-first namespaces)

(defn- classifications-on-classpath [prefix]
  (let [roots (map io/file (ecosystem/leiningen-paths))
        selecteds (map #(tude/select-subdirectory % prefix) roots)
        dirnames (map #(.getPath ^java.io.File %) selecteds)]
    (mapcat tude/classify-dir-entries dirnames)))

(defn unglob-partial-namespaces [namespaces]
  (mapcat #(if (= \* (last %))
             (valid-namespace-symbols (classifications-on-classpath (apply str (butlast %))))
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

(defonce state-tracker-atom (atom (nstrack/tracker)))

(defn file-modification-time [^java.io.File file]
  (.lastModified file))

(defn latest-modification-time [state-tracker]
  (let [ns-to-file (map/invert (filemap-key state-tracker))
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
              [new-roots unkilled-descendents] (seq/bifurcate actual-dependent-set surviving-namespaces)]
          (recur (concat roots-to-handle-later new-roots)
                 unkilled-descendents))))))

(defn react-to-tracker! [state-tracker options]
  (let [namespaces (load-key state-tracker)]
    (when (not (empty? namespaces))
      ( (:namespace-stream-checker options)
        namespaces
        #(require-namespaces! namespaces
                              (:on-require-failure options)
                              (mkfn:clean-dependents state-tracker)))
      (println (color/note (format "[Completed at %s]"
                                   (time/format-local-time (time/local-now) :hour-minute-second)))))))

(defn prepare-for-next-scan [state-tracker]
  (let [unloaded (clojure.set/difference (set (unload-key state-tracker))
                                         (set (load-key state-tracker)))
        remove-unloaded (fn [dependents] (apply disj dependents unloaded))]
    (-> state-tracker
        (assoc time-key (latest-modification-time state-tracker)
               unload-key []
               load-key [])
        (update-in [deps-key :dependents]
                   map/update-each-value remove-unloaded))))

(defn with-fresh-copy-of-dependency-map
  "Records must be recreated if the protocols that they implement are reloaded."
  [state]
  (if (get state deps-key)
    (update-in state [deps-key] clojure.tools.namespace.dependency/map->MapDependencyGraph)
    state))

(defn mkfn:scan-and-react [options scanner]
  (fn []
    (swap! state-tracker-atom
           #(let [state (with-fresh-copy-of-dependency-map %)
                  new-tracker (apply scanner state (:files options))]
              (react-to-tracker! new-tracker options)
              (prepare-for-next-scan new-tracker)))))


(defn mkfn:react-to-changes [options]
  (mkfn:scan-and-react options nsdir/scan))

(defn load-everything [options]
  ((mkfn:scan-and-react options nsdir/scan-all)))
