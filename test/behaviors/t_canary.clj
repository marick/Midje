;; -*- indent-tabs-mode: nil -*-

;;; Not wildly important, these tests are older descriptions of
;;; bugs. They've been fixed, and more specific tests have been
;;; written, but no harm in keeping them around. Delete if they start
;;; to annoy.


(ns behaviors.t-canary
  (:use [midje.sweet])
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

(unfinished f)
(against-background [ (f 1) => 2 ]
  (after-silently 
   (fact "prerequisites can be used within checks"
     (against-background (around :checks (let [x 1] ?form)))
     (+ (f x) 2) => 4)
   (fact (only-passes? 1) => truthy)))

(defn day-of-month-stream [xyz] :blah)

(defn new-date [x y z] :blah)

(tabular
  (fact "makes a stream of given day of the month"
    (day-of-month-stream 1) => ?date-time)
	  
	?nth   ?date-time
        first  (new-date 1 2 3))

(defn today-num [])

(tabular 
 (fact 
   #(today-num) => anything)
 ?day-num
 7 )
