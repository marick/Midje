(ns ^{:doc "Emissions change global state: the output and the pass/fail record."}
  midje.emission.api
  (:require [midje.ideas.reporting.report :as report]
            [midje.clojure-test-facade :as ctf]
            [midje.ideas.reporting.levels :as levelly]
            [midje.emission.state :as state]))

(defn load-plugin [location]
  (if (symbol? location)
    (require location :reload)
    (load-file location)))

(defn- bounce
  ([keyword args]
     (let [function (keyword state/emission-functions)]
       (if function
         (apply function args)
         (throw (Error. (str "Your emission plugin does not define " keyword))))))
  ([keyword]
     (bounce keyword [])))

(defmacro just-bounce [symbol]
  `(defn ~symbol [& args#]
     (bounce ~(keyword symbol) args#)))

(defn pass []
  (state/output-counters:inc:midje-passes!)
  (ctf/note-pass);; TODO: TEMPORARY
  (bounce :pass))

(defn fail [report-map]
  (state/output-counters:inc:midje-failures!)
  (bounce :fail [report-map]))
  
(defn forget-everything []
  (state/reset-output-counters!)
  (ctf/zero-counters)  ;; TODO This is temporary until clojure.test is vanquished.
  (bounce :forget-everything))

;; TODO: For the time being, this includes clojure.test failures
;; Once Midje emissions are completely separated from clojure.test
;; reporting, that can go away.
(defn midje-failures []
  (+ (state/output-counters:midje-failures) (:fail (ctf/counters))))
