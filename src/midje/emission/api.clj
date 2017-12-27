(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.emission.api
  (:require [midje.config :as config]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.levels :as levels]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as silence]))

;;; Level handling

(defn level-checker [operation]
  (fn [level-name]
    (operation (levels/normalize (config/choice :print-level))
               (levels/normalize level-name))))

(def config-at-or-above? (level-checker >=))
(def config-above?(level-checker >))

;;; Plugins

(defn load-plugin [location]
  (if (symbol? location)
    (require location :reload)
    (load-file location)))

(defn- bounce-to-plugin
  [keyword & args]
  (let [function (keyword state/emission-functions)]
    (if function
      (apply function args)
      (throw (Error. (str "Your emission plugin does not define " keyword state/emission-functions))))))

;;; The API proper

(defn pass []
  (state/output-counters:inc:midje-passes!)
  (ctf/note-pass)
  (when (config-above? :print-nothing) (bounce-to-plugin :pass)))

(defn fail-silently
  "Use this to affect the final summary without producing output."
  []
  (state/output-counters:inc:midje-failures!)
  (ctf/note-fail))

(defn fail [failure-map]
  (fail-silently)
  (state/add-raw-fact-failure! failure-map)
  (when (config-above? :print-nothing) (bounce-to-plugin :fail failure-map)))

(defn info [lines]
  (when (config-above? :print-nothing) (bounce-to-plugin :info lines)))

(defn future-fact [description position]
  (when (and (config-above? :print-nothing)
             (config/choice :visible-future))
    (bounce-to-plugin :future-fact description position)))

(defn forget-everything []
  (bounce-to-plugin :starting-fact-stream)
  ;; This happens after the `forget-everything` so that
  ;; a plugin can capture old values if it wants to.
  (state/reset-output-counters!))


(defn starting-to-check-top-level-fact [fact-function]
  (state/forget-raw-fact-failures!)
  (when (config-at-or-above? :print-facts)
    (bounce-to-plugin :starting-to-check-top-level-fact fact-function)))

(defn finishing-top-level-fact [fact-function]
  (ctf/note-test)
  (when (config-at-or-above? :print-facts)
    (bounce-to-plugin :finishing-top-level-fact fact-function)))

(defn starting-to-check-fact [fact-function]
  (when (config-at-or-above? :print-facts)
    (bounce-to-plugin :starting-to-check-fact fact-function)))

(defn finishing-fact [fact-function]
  (when (config-at-or-above? :print-facts)
    (bounce-to-plugin :finishing-fact fact-function)))

(defn possible-new-namespace [ns]
  (when (config-at-or-above? :print-namespaces)
    (bounce-to-plugin :possible-new-namespace ns)))

(defn fact-stream-summary
  ([clojure-test-results]
     (when (config-above? :print-no-summary)
       (bounce-to-plugin :finishing-fact-stream (state/output-counters) clojure-test-results)))
  ([]
     (fact-stream-summary {:test 0})))

;;;

(defmacro producing-only-raw-fact-failures [& body]
  `(config/at-print-level :print-nothing
      (state/with-isolated-output-counters
        ~@body)))

(defmacro silently [& body]
  `(producing-only-raw-fact-failures ~@body))
