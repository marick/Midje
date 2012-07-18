(ns ^{:doc "Renders the various reported fact evaluation results."}
  midje.ideas.reporting.report
  (:use clojure.test
        clojure.test.junit
        [midje.ideas.reporting.string-format :only [report-strings-format-config]]
        [midje.ideas.reporting.junit-xml-format :only [junit-xml-format-config]]))


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
          (->> m 
               ((:single-fact-fn *report-format-config*)) 
               flatten 
               (remove nil?) 
               (map *renderer*) 
               doall))]

  (defmethod clojure.test/report :default [m]
    (inc-report-counter :fail)
    (note-failure-in-fact)
    (render m))

  (defmethod clojure.test/report :future-fact [m]
    (render m))

  (defmethod clojure.test.junit/junit-report :default [m]
    (inc-report-counter :fail)
    (note-failure-in-fact)
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
  )
