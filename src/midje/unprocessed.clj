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
  (:require [midje.emission.boundaries :as emission-boundary]
            [midje.emission.api :as emit]))

(immigrate 'midje.checkers)

(letfn [(result-discovered-by-a-function? [check-map] (extended-fn? (:expected-result check-map)))

        (minimal-failure-map [type actual check-map]
          {:type type
           :description (:description check-map)
           :binding-note (:binding-note check-map)
           :position (:position check-map)
           :actual actual
           :expected-result-form (:expected-result-form check-map)}) ;; TODO: delete this

        (check-result-positive [actual check-map]
          (cond  (extended-= actual (:expected-result check-map))
                 (emit/pass)
                 
                 
                 (result-discovered-by-a-function? check-map)
                 (emit/fail (merge (minimal-failure-map :mock-expected-result-functional-failure
                                         actual check-map)
                                   ;; TODO: It is very lame that the
                                   ;; result-function has to be called again to
                                   ;; retrieve information that extended-=
                                   ;; knows and threw away. But it's surprisingly
                                   ;; difficult to use evaluate-checking-function
                                   ;; at the top of the cond
                                   (second (evaluate-checking-function (:expected-result check-map)
                                                                       actual))))
                 
                 :else
                 (emit/fail (assoc (minimal-failure-map :mock-expected-result-failure actual check-map)
                                   :expected-result (:expected-result check-map)))))
        
        (check-result-negated [actual check-map]
          (cond (not (extended-= actual (:expected-result check-map)))
                (emit/pass)
                
                (result-discovered-by-a-function? check-map)
                (emit/fail (minimal-failure-map :mock-actual-inappropriately-matches-checker actual check-map))
                
                :else
                (emit/fail (minimal-failure-map :mock-expected-result-inappropriately-matched actual check-map))))]


  (defn- check-result [actual check-map]
    (if (= (:desired-check check-map) :check-match)
      (check-result-positive actual check-map)
      (check-result-negated actual check-map)))

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
