;;; Not wildly important, these tests are older descriptions of
;;; bugs. They've been fixed, and more specific tests have been
;;; written, but no harm in keeping them around. Delete if they start
;;; to annoy.


(ns midje.sweet.t-canary-tests
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util]))


(unfinished favorite-animal)
(defn favorite-animal-name [] (name (favorite-animal)))
(defn favorite-animal-empty [] )
(defn favorite-animal-only-animal [] (favorite-animal))
(defn favorite-animal-only-name [] (name "fred"))

(fact
     (favorite-animal-name) => "betsy"
     (provided
       (name (favorite-animal)) => "betsy"))
			  





(unfinished all-procedures)
(def ...procedure... '...procedure...)
(defn build-map [exclusion-function]
  (reduce (fn [accumulator name] (conj accumulator {name (exclusion-function name)}))
	  {}
	  (all-procedures)))
(defn all-procedures-exclude-nothing []
  (build-map (fn [procedure] [])))

 (fact "unless otherwise stated, each procedure excludes no animals"
   (all-procedures-exclude-nothing) => { ...procedure... [] }
   (provided
     (all-procedures) => [...procedure...] ))



(unfinished first-subfunction another-subfunction)
  
(defn function-under-test []
  (+ (first-subfunction 1 2 '(a blue cow))
     (another-subfunction inc)))

(fact (function-under-test) => 11
        (provided
          (first-subfunction odd? even? anything) => 1
	  (another-subfunction (exactly inc)) => 10))

