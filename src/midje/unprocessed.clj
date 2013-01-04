(ns ^{:doc "Core Midje functions that process expects and report on their results."} 
  midje.unprocessed 
  (:use clojure.test
        [midje.ideas.background :only [background-fakes]]
        [midje.checkers.extended-equality :only [extended-= evaluate-checking-function]]
        [midje.error-handling.exceptions :only [captured-throwable]]
        midje.internal-ideas.fakes
        midje.util.laziness
        [midje.util.form-utils :only [extended-fn?]]
        [midje.util.namespace :only [immigrate]])
  (:require [midje.internal-ideas.emission-boundaries :as emission-boundary]
            [midje.internal-ideas.emissions :as emit]))
(immigrate 'midje.checkers)

(letfn [(result-discovered-by-a-function? [call] (extended-fn? (:expected-result call)))

        (minimal-failure-map [type actual call]
          {:type type
           :description (:description call)
           :binding-note (:binding-note call)
           :position (:position call)
           :actual actual
           :expected (:expected-result-text-for-failures call)})

        (check-result-positive [actual call]
          (cond  (extended-= actual (:expected-result call))
                 (emit/pass)
                 
                 
                 (result-discovered-by-a-function? call)
                 (emit/fail (merge (minimal-failure-map :mock-expected-result-functional-failure
                                         actual call)
                                   ;; TODO: It is very lame that the
                                   ;; result-function has to be called again to
                                   ;; retrieve information that extended-=
                                   ;; knows and threw away. But it's surprisingly
                                   ;; difficult to use evaluate-checking-function
                                   ;; at the top of the cond
                                   (second (evaluate-checking-function (:expected-result call)
                                                                       actual))))
                 
                 :else
                 (emit/fail (assoc (minimal-failure-map :mock-expected-result-failure actual call)
                                   :expected (:expected-result call)))))
        
        (check-result-negated [actual call]
          (cond (not (extended-= actual (:expected-result call)))
                (emit/pass)
                
                (result-discovered-by-a-function? call)
                (emit/fail (minimal-failure-map :mock-actual-inappropriately-matches-checker actual call))
                
                :else
                (emit/fail (minimal-failure-map :mock-expected-result-inappropriately-matched actual call))))]


  (defn- check-result [actual call]
    (cond (= (:desired-check call) :check-match)
          (check-result-positive actual call)
          
          :else
          (check-result-negated actual call)))

)
(defn expect*
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  [check-map local-fakes]
  (with-installed-fakes (concat (reverse (filter :data-fake (background-fakes))) local-fakes)
    (emission-boundary/around-check 
      (let [actual (try  
                     (eagerly ((:function-under-test check-map)))
                    (catch Throwable ex
                      (captured-throwable ex)))]
        (report-incorrect-call-counts local-fakes)
        (check-result actual check-map)
        :irrelevant-return-value))))
