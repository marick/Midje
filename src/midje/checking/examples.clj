(ns ^{:doc "Core Midje functions that process expects and report on their results."} 
  midje.checking.examples
  (:use clojure.test
        [midje.checkers.extended-equality :only [extended-= evaluate-checking-function]]
        [midje.error-handling.exceptions :only [captured-throwable]]
        midje.internal-ideas.fakes
        midje.util.form-utils
        midje.util.laziness
        [midje.util.namespace :only [immigrate]])
  (:require [midje.emission.boundaries :as emission-boundary]
            [midje.parsing.background :as background]
            [midje.emission.api :as emit]))

(immigrate 'midje.checkers)

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

(defn check-one
  "Takes a map describing a single example, plus some function redefine-maps
   and checks that example, reporting results through the emission interface."
  [example-map local-fakes]
  (with-installed-fakes (concat (reverse (filter :data-fake (background/background-fakes))) local-fakes)
    (emission-boundary/around-check 
      (let [actual (try  
                     (eagerly ((:function-under-test example-map)))
                    (catch Throwable ex
                      (captured-throwable ex)))]
        (report-incorrect-call-counts local-fakes)
        (check-result actual example-map)
        :irrelevant-return-value))))
