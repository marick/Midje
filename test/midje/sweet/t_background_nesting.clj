(ns midje.sweet.t-background-nesting
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(deftest background-command-slams-new-background-in-place
  (in-separate-namespace
   (background (outermost ...o...) => 1)
   (fact (+ 1 (outermost ...o...)) => 2)
   (background (outermost ...o...) => -1)
   (fact (+ 1 (outermost ...o...)) => 0)))

(deftest background-command-is-shadowed-by-against-background
  (in-separate-namespace
   (background (outermost) => 2
	       (middlemost) => 'a)
   (against-background [ (middlemost) => 33]
		       (fact (+ (middlemost) (outermost)) => 35))))
  

(deftest three-levels-of-nesting-one-duplicated
  (in-separate-namespace
   (background (outermost) => 2
	       (middlemost) => 'a)
   (against-background [ (middlemost 2) => 33
			 (innermost) => 'c]

     (against-background [ (middlemost 1) => -43 ]
       (fact
	 (against-background (innermost) => 8)
	 (+ (middlemost 2) (middlemost 1) (outermost) (innermost)) => 0)))))


(against-background [ (middlemost) => 33 ]
  (deftest backgrounds-span-deftests
    (fact
      (against-background (innermost) => 8)
      (+ (middlemost) (innermost)) => 41)))
     
		      
(deftest left-to-right-shadowing
  (against-background [ (middlemost) => 33 (middlemost) => 12]
    (fact (* 2 (middlemost)) => 24)))
  
(in-separate-namespace
 (against-background [ (middlemost) => "FOO!" ]
   (try 
     (against-background [ (middlemost) => 33 ]
       (fact (middlemost) => 33)
       (throw (Throwable.)))
     (catch Throwable ex))
   (fact (middlemost) => "FOO!")))


(in-separate-namespace
 (fact (map? {1 'do}) => truthy)
 (fact (first (second '(midje.semi-sweet.expect (midje.sweet.fact 1 => 2)))) => 'midje.sweet.fact)
 (fact (set? #{1 'do}) => truthy)


 (against-background [ (middlemost ...one...) => 1 ]
   (let [two 2]
     (facts
       (vector? [1 two]) => truthy
       (let [three 3]
	 (+ (middlemost ...one...) two three) => 6)))))
