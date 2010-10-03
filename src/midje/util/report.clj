(when (= (class clojure.test/report) clojure.lang.MultiFn)
  (eval
   '(do (require 'clojure.test)
        (ns clojure.test)
        (defonce old-report clojure.test/report))))

(ns midje.util.report
    (:use clojure.test))

(defn- midje-position-string [position-pair]
  (format "(%s:%d)" (first position-pair) (second position-pair)))

(defmethod clojure.test/old-report :mock-argument-match-failure [m]
;  (with-test-out
   (inc-report-counter :fail)
   (println "\nFAIL at" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "You never said" (:name (meta (:function m))) "would be called with these arguments:")
   (println (pr-str (:actual m))))

(defmethod clojure.test/old-report :mock-incorrect-call-count [m]
;  (with-test-out
   (inc-report-counter :fail)
   (println "\nFAIL for" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "This expectation was never satisfied:")
   (println (:expected m) "should be called at least once."))


(defmethod clojure.test/old-report :mock-expected-result-failure [m]
;  (with-test-out
   (inc-report-counter :fail)
   (println "\nFAIL at" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "expected:" (pr-str (:expected m)))
   (println "  actual:" (pr-str (:actual m))))

(defmethod clojure.test/old-report :mock-expected-result-functional-failure [m]
;  (with-test-out
   (inc-report-counter :fail)
   (println "\nFAIL at" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "Actual result did not pass expected function.")
   (println "expected function:" (pr-str (:expected m)))
   (println "    actual result:" (pr-str (:actual m))))

