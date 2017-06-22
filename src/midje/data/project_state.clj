(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.data.project-state
  (:require [clj-time.local :as time]
            [clojure.java.io :as io]
            [clojure.set]
            [clojure.tools.namespace.dependency :as nsdependency]
            [clojure.tools.namespace.repl :as nsrepl]
            [clojure.tools.namespace.dir :as nsdir]
            [clojure.tools.namespace.track :as nstrack]
            [clojure.tools.namespace.reload :as nsreload]
            [commons.clojure.core :refer :all :exclude [any?]]
            [midje.config :as config]
            [midje.emission.api :as emit]
            [midje.emission.boundaries :as emission-boundary]
            [midje.emission.colorize :as color]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.bultitude :as tude]
            [such.maps :as map]
            [such.sequences :as seq]))

;;; Querying the project tree

(defn valid-namespace-symbols [classifieds]
  (letfn [(filenames [fileseq]
            (prn (:file (first fileseq)))
            (prn (.getName (:file (first fileseq))))
            (into [] (map #(.getName (:file %)) fileseq)))]
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

(defn classifications-on-classpath [prefix]
  (let [roots (map io/file (ecosystem/leiningen-paths))
        selecteds (map #(tude/select-subdirectory % prefix) roots)
        dirnames (map #(.getPath %) selecteds)]
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
(def error-key :clojure.tools.namespace.reload/error)
(def error-ns-key :clojure.tools.namespace.reload/error-ns)

;; Global state.

(defonce state-tracker-atom (atom (nstrack/tracker)))

(defn file-modification-time [file]
  (.lastModified file))

(defn latest-modification-time [state-tracker]
  (let [ns-to-file (map/invert (filemap-key state-tracker))
        relevant-files (map ns-to-file (load-key state-tracker))]
    (apply max (time-key state-tracker)
           (map file-modification-time relevant-files))))


(defn require-namespaces! [tracker on-require-failure]
  (let [reloaded-tracker (nsreload/track-reload tracker)]
    (when-let [error (error-key reloaded-tracker)]
      (on-require-failure (error-ns-key reloaded-tracker) error))))

(defn react-to-tracker! [state-tracker options]
  (let [namespaces       (load-key state-tracker)
        namespace-loader (fn [] (require-namespaces! state-tracker
                                                     (:on-require-failure options)))]

    (when (not (empty? namespaces))
      ((:namespace-stream-checker options) namespaces namespace-loader)
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
    (update-in state [deps-key] nsdependency/map->MapDependencyGraph)
    state))

(defn mkfn:scan-and-react [options scanner]
  (fn []
    (let [state       (with-fresh-copy-of-dependency-map @state-tracker-atom)
          new-tracker (apply scanner state (:files options))]
      (react-to-tracker! new-tracker options)
      (reset! state-tracker-atom (prepare-for-next-scan new-tracker)))))


(defn mkfn:react-to-changes [options]
  (mkfn:scan-and-react options nsdir/scan))

(defn load-everything [options]
  ((mkfn:scan-and-react options nsdir/scan-all)))
