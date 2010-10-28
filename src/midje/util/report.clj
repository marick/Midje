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
   (inc-report-counter :fail)
   (println "\nFAIL at" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "You never said" (:name (meta (:function m))) "would be needed with these arguments:")
   (println (pr-str (:actual m))))

(defmethod clojure.test/old-report :mock-incorrect-call-count [m]
   (inc-report-counter :fail)
   (println "\nFAIL for" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "You claimed the following was needed, but it was never used:")
   (println (:expected m)))


(defmethod clojure.test/old-report :mock-expected-result-failure [m]
   (inc-report-counter :fail)
   (println "\nFAIL at" (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (println (testing-contexts-str)))
   (println "Expected:" (pr-str (:expected m)))
   (println "  Actual:" (pr-str (:actual m))))

(def *renderer* println)

(defmacro with-identity-renderer [& forms]   ; for testing
  `(binding [*renderer* identity] ~@forms))

(defn flatten-and-remove-nils [seq]
  (filter identity (flatten seq)))

(defmulti report-strings :type)

(defmethod report-strings :mock-expected-result-functional-failure [m]
  (list
   (str "\nFAIL at " (midje-position-string (:position m)))
   (when (seq *testing-contexts*) (testing-contexts-str))
   "Actual result did not agree with the checking function."
   (str "    Actual result: " (pr-str (:actual m)))
   (str "Checking function: " (pr-str (:expected m)))
   (if (:actual-processor m)
     (list 
      (str "During checking, the function applied #'" (:name (meta (:actual-processor m))) " to the result.")
      (str "It was surprised to see " (pr-str (:processed-actual m)) ".")))))
  
(defn render [m]
  (doall (map *renderer* (flatten-and-remove-nils (report-strings m)))))

(defmethod clojure.test/old-report :mock-expected-result-functional-failure [m]
   (inc-report-counter :fail)
   (render m))
