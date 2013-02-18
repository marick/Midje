(ns ^{:doc "Core Midje functions that process expects and report on their results."} 
  midje.checking.predictions
  (:use midje.clojure.core
        midje.checking.core
        [midje.util.exceptions :only [captured-throwable]]
        midje.data.prerequisite-state
        midje.util.laziness)
  (:require [midje.config :as config]
            [midje.emission.boundaries :as emission-boundary]
            [midje.parsing.1-to-explicit-form.background :as background]
            [midje.emission.api :as emit]))

(defn- minimal-failure-map
  "Failure maps are created by adding on to parser-created maps"
  [type actual existing]
  (assoc existing :type type :actual actual))

(def ^{:private true} has-function-checker? (comp extended-fn? :expected-result))

(defn- check-for-match [actual example-map]
  (cond  (extended-= actual (:expected-result example-map))
         (emit/pass)
         
         (has-function-checker? example-map)
         (emit/fail (merge (minimal-failure-map :actual-result-did-not-match-checker
                                                          actual example-map)
                           ;; TODO: It is very lame that the
                           ;; result-function has to be called again to
                           ;; retrieve information that extended-=
                           ;; knows and threw away. But it's surprisingly
                           ;; difficult to use evaluate-checking-function
                           ;; at the top of the cond
                           (second (evaluate-checking-function (:expected-result example-map)
                                                               actual))))
         
         :else
         (emit/fail (assoc (minimal-failure-map :actual-result-did-not-match-expected-value actual example-map)
                           :expected-result (:expected-result example-map)))))


(defn- check-for-mismatch [actual example-map]
  (cond (not (extended-= actual (:expected-result example-map)))
        (emit/pass)
        
        (has-function-checker? example-map)
        (emit/fail (minimal-failure-map :actual-result-should-not-have-matched-checker actual example-map))
        
        :else
        (emit/fail (minimal-failure-map :actual-result-should-not-have-matched-expected-value actual example-map))))


(defn- check-result [actual example-map]
  (if (= (:check-expectation example-map) :expect-match)
    (check-for-match actual example-map)
    (check-for-mismatch actual example-map)))



(defmulti call-count-incorrect? :type)

(defmethod call-count-incorrect? :fake [fake]
  (let [method (:times fake)
        count @(:call-count-atom fake)]
    (pred-cond method 
      #(= % :default) (zero? count)
      number?         (not= method count)
      coll?           (not-any? (partial = count) method)
      fn?             (not (method count)))))

(defmethod call-count-incorrect? :not-called [fake]
  (not (zero? @(:call-count-atom fake))))

(defn report-incorrect-call-counts [fakes]
  (when-let [failures (seq (for [fake fakes
                                 :when (call-count-incorrect? fake)]
                              {:actual-count    @(:call-count-atom fake)
                               :expected-count  (:times fake)
                               :expected-call   (:call-text-for-failures fake)
                               :position        (:position fake)
                               :expected-result-form        (:call-text-for-failures fake)}))]
    (emit/fail {:type :some-prerequisites-were-called-the-wrong-number-of-times
                :failures failures
                :position (:position (first failures))} )))





(defn check-one
  "Takes a map describing a single example, plus some function redefine-maps
   and checks that example, reporting results through the emission interface."
  [example-map local-fakes]
  ((config/choice :check-recorder) example-map local-fakes)
  (with-installed-fakes (concat (reverse (filter :data-fake (background/background-fakes)))
                                local-fakes
                                (remove :data-fake (background/background-fakes)))
    (emission-boundary/around-check 
      (let [actual (try  
                     (eagerly ((:function-under-test example-map)))
                    (catch Throwable ex
                      (captured-throwable ex)))]
        (report-incorrect-call-counts local-fakes)
        (check-result actual example-map)
        :irrelevant-return-value))))
