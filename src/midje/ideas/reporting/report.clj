(ns ^{:doc "Renders the various reported fact evaluation results."}
  midje.ideas.reporting.report
  (:use clojure.test
        [midje.ideas.reporting.string-format :only [report-strings]]))

(intern (the-ns 'clojure.test) 'old-report clojure.test/report)

(def #^:dynamic #^:private *renderer* println)


;;; This mechanism is only used to make `fact` return appropriate values of
;;; true or false. It doesn't piggyback off clojure.test/*report-counters*
;;; partly because that's not normally initialized and partly to reduce
;;; dependencies.

(def #^:dynamic #^:private *failure-in-fact* false)

(defn note-failure-in-fact
  ([] (note-failure-in-fact true))
  ([val] (alter-var-root #'*failure-in-fact* (constantly val))))

(defn- fact-begins []
  (note-failure-in-fact false))

(defn- fact-checks-out? []
  (not *failure-in-fact*))

(defn form-providing-friendly-return-value [test-form]
  `(do 
     (#'fact-begins)
     ~test-form
     (#'fact-checks-out?)))
  
(letfn [(render [m]
          (->> m report-strings flatten (remove nil?) (map *renderer*) doall))]

  (defmethod clojure.test/old-report :default [m]
    (inc-report-counter :fail )
    (note-failure-in-fact)
    (render m))

  (defmethod clojure.test/old-report :future-fact [m]
    (render m)))
