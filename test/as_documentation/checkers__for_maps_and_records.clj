(ns as-documentation.checkers--for-maps-and-records
  (:use midje.sweet
        midje.test-util))


                                ;;; Implications of extended equality

(defrecord R [x y])
(defrecord NotR [x y])

(facts "about maps and records on the right-hand side"
  (fact "using a map implies that you care about contents, not type"
    (R. 1 2) => {:x 1, :y 2})
  (fact "using a record implies that you care about both contents and type"
    {:x 1, :y 1} =not=> (R. 1 2)
    (NotR. 1 2)  =not=> (R. 1 2)
    (R. 1 2)     =>     (R. 1 2)))


                                ;;; Just


(fact "`just` provides extended equality"
  {:a 1, :b 2, :c "some text"} => (just {:a odd?, :b 2, :c #"text"}))


                                ;;; contains

(fact "contains ignores unmentioned keys"
  (R. 1 'IGNORE!) => (contains {:x 1}))

(fact "`contains` provides extended equality"
  (R. 1 'IGNORE) => (contains {:x odd?}))
  
                                ;;; has

(fact "`'has` quantifies over values"
  {:a 1, :b 3} => (has every? odd?))


                                ;;; key/value pairs

(fact "a sequence of key/value pairs is OK on the right-hand side"
  {:a 1, :b 2} => (just [[:a 1] [:b 2]])
  (R. 1 nil) => (contains [[:x 1]]))
