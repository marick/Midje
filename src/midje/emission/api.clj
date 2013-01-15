(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.emission.api
  (:use [midje.util.thread-safe-var-nesting :only [with-altered-roots]])
  (:require [midje.clojure-test-facade :as ctf]
            [midje.config :as config]
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
      (throw (Error. (str "Your emission plugin does not define " keyword))))))

;;; The API proper

(defn pass []
  (state/output-counters:inc:midje-passes!)
  (when (config-above? :print-nothing) (bounce-to-plugin :pass)))

(defn fail [failure-map]
  (state/add-raw-fact-failure! failure-map)
  (state/output-counters:inc:midje-failures!)
  (when (config-above? :print-nothing) (bounce-to-plugin :fail failure-map)))

(defn future-fact [description position]
  (when (and (config-above? :print-nothing)
             (config/choice :visible-future))
    (bounce-to-plugin :future-fact description position)))
  
(defn forget-everything []
  (state/reset-output-counters!)
  (bounce-to-plugin :forget-everything))

(defn starting-to-check-fact [fact-function]
  (state/forget-raw-fact-failures!)
  (when (config-at-or-above? :print-facts)
    (bounce-to-plugin :starting-to-check-fact fact-function)))

(defn possible-new-namespace [ns]
  (when (config-at-or-above? :print-namespaces)
    (bounce-to-plugin :possible-new-namespace ns)))

(defn fact-stream-summary
  ([clojure-test-results]
     (when (config-above? :print-no-summary)
       (bounce-to-plugin :fact-stream-summary (state/output-counters) clojure-test-results)))
  ([]
     (fact-stream-summary {:test 0})))
                                 
;;; 

(defmacro producing-only-raw-fact-failures [& body]
  `(config/at-print-level :print-nothing
      (state/with-isolated-output-counters
        ~@body)))

(defmacro silently [& body]
  `(producing-only-raw-fact-failures ~@body))
