(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.emission.api
  (:require [midje.ideas.reporting.report :as report]
            [midje.clojure-test-facade :as ctf]
            [midje.config :as config]
            [midje.emission.levels :as levels]
            [midje.emission.state :as state]))

;; TODO: For the time being, this includes clojure.test failures
;; Once Midje emissions are completely separated from clojure.test
;; reporting, that can go away.
(defn midje-failures []
  (+ (state/output-counters:midje-failures) (:fail (ctf/counters))))

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
  (ctf/note-pass);; TODO: TEMPORARY
  (when (config-above? :print-nothing) (bounce-to-plugin :pass)))

(defn fail [report-map]
  (state/output-counters:inc:midje-failures!)
  (when (config-above? :print-nothing) (bounce-to-plugin :fail report-map)))
  
(defn forget-everything []
  (state/reset-output-counters!)
  (ctf/zero-counters)  ;; TODO This is temporary until clojure.test is vanquished.
  (bounce-to-plugin :forget-everything))

(defn starting-to-check-fact [fact-function]
  (when (config-at-or-above? :print-facts) (bounce-to-plugin :starting-to-check-fact fact-function)))

(defn possible-new-namespace [ns]
  (when (config-at-or-above? :print-namespaces) (bounce-to-plugin :possible-new-namespace ns)))
