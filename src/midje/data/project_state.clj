(ns ^{:doc "What we know about the changing project file/namespace tree."}
  midje.data.project-state
  (:require [clj-time.local :as time]
            [clojure.java.io :as io]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.track :as track]
            [clojure.tools.namespace.repl :as repl]
            [commons.clojure.core :refer :all :exclude [any?]]
            [midje.emission.api :as emit]
            [midje.emission.colorize :as color]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.bultitude :as tude]))

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

(defonce state-tracker-atom (atom (track/tracker)))

(defn- react-to-tracker! [state-tracker options]
  (let [namespaces       (:clojure.tools.namespace.track/load state-tracker)
        namespace-loader (fn [] (let [refresh-result (repl/refresh)
                                      on-failure     (:on-require-failure options)]
                                  (when-not (= :ok refresh-result)
                                    (on-failure refresh-result))))]
    (when (not (empty? namespaces))
      ((:namespace-stream-checker options) namespaces namespace-loader)
      (let [time-now (time/format-local-time (time/local-now) :hour-minute-second)]
        (println (color/note (format "[Completed at %s]" time-now)))))))

(let [prev-failed (atom nil)]
  (defn- scan-for-changes [scanner tracker watch-dirs]
    (try (let [new-tracker (apply scanner tracker watch-dirs)]
           (reset! prev-failed false)
           new-tracker)
         (catch Exception e
           (when-not @prev-failed
             (println e))
           (reset! prev-failed true)
           ;; return the same tracker so we dont try to run tests
           tracker))))

(defn- mkfn:scan-and-react [options scanner]
  (fn []
    (let [old-tracker @state-tracker-atom
          new-tracker (scan-for-changes scanner old-tracker (:files options))]
      (when-not (= new-tracker old-tracker)
        (react-to-tracker! new-tracker options)
        (reset! state-tracker-atom
                (dissoc new-tracker
                        :clojure.tools.namespace.track/load
                        :clojure.tools.namespace.track/unload))))))

(defn mkfn:react-to-changes [options]
  (mkfn:scan-and-react options dir/scan))

(defn load-everything [options]
  ((mkfn:scan-and-react options dir/scan-all)))
