(ns ^{:doc "Renders the various reported fact evaluation results."}
  midje.ideas.reporting.report
  (:use clojure.test
        clojure.test.junit
        [midje.ideas.reporting.string-format :only [report-strings-format-config]]
        [midje.ideas.reporting.junit-xml-format :only [junit-xml-format-config]])
  (:require [midje.config :as config]))


;;; Configuration

;; *report-format-config* expects to be bound to a map like:
;; { :single-fact-fn (fn [reported-map] ...)   ; handle each failing fact
;;   :summary-fn (fn [exit-after-tests?] ...)  ; output a summary of successes/failures etc  
;; }

(def ^{:dynamic true
       :doc "Midje will report the fact failures and summary using the 
             fns in the configuration map bound to this var."}
  *report-format-config* report-strings-format-config)

(def formatters { "default" report-strings-format-config
                  "junit-xml" junit-xml-format-config })


;;; Reporting

(def #^:dynamic #^:private *renderer* println)


;; This turns off "Testing ...." lines, which I hate, especially
;; when there's no failure output. The type check is because
;; `lein test` overrides clojure.test/report with a non-multimethod.
(when (= clojure.lang.MultiFn (type clojure.test/report))
  (defmethod clojure.test/report :begin-test-ns [m]))


(defn render [m]
  (when (config/above? :print-nothing)
    (->> m 
         ((:single-fact-fn *report-format-config*)) 
         flatten 
         (remove nil?) 
         (map *renderer*) 
         doall)))

(defmethod clojure.test/report :default [m]
  (inc-report-counter :fail)
  (render m))

(defmethod clojure.test/report :future-fact [m]
  (render m))

(defmethod clojure.test.junit/junit-report :default [m]
  (inc-report-counter :fail)
  (try
    (with-test-out
      (failure-el (:description m)
                  (:expected m)
                  (:actual m)))
    (catch Exception e
      (.printStackTrace e)
      (throw e)))
  (render m))


(defmethod clojure.test.junit/junit-report :future-fact [m]
  (render m))

(defmethod clojure.test.junit/junit-report :summary [m])
