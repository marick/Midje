;; -*- indent-tabs-mode: nil -*-

(ns midje.t-checkers
  (:use midje.sweet
        midje.test-util))

;; Make sure the published interface actually exists.

(fact "simple checkers exist"
  1 => truthy
  nil => falsey
  'foo => anything
  'foo => any
  odd? => (exactly odd?)
  (throw (Error.)) => (throws Error))

(fact "deprecated checkers"
  [1 2 3] => (in-any-order [3 2 1])
  {:a 1 :b 2} => (map-containing {:a 1})
  [{:a 1} {:b 2}] => (maps-containing {:a 1} {:b 2})
  [{:a 1} {:b 2}] => (only-maps-containing {:a 1} {:b 2}))

(defn equality [expected]
  (chatty-checker [actual] (= actual expected)))

(fact "chatty checkers exist"
  1 => (equality 1))

(fact "collection checkers"
  [1] => (contains 1)
  [1] => (just 1)
  [1] => (has-prefix 1)
  [1] => (has-suffix 1)
  [1] => (has every? odd?))
    


(defn as-sets [& expected]
  (let [set-of-sets #(set (map set %))]
    (fn [actual]
      ( (just (set-of-sets expected)) (set-of-sets actual)))))

(after-silently 
(fact 
   [ [1] [2 3] ] => (as-sets [ [1] ]))
(future-fact "Failures from chatty-checkers-within-functions propagate chatty information"
   (first @reported) => (contains {:type :mock-expected-result-functional-failure
                                   :actual [ [1] [2 3]]
                                   :expected '(as-sets [[1]])
                                   :notes  ["Expected one element. There were two."]})))


;; The behavior of checkers is different in prerequisites

(tabular
  (fact ?actual ?arrow ?expected)
  
  ?expected                      ?actual ?arrow
  odd?                           3       =>             ;; why does this ONLY blwo up when I uncomment all rows of the table?
  odd?                           odd?    =not=>
                                                        ;; fuck an a dude.
  (exactly odd?)                 3       =not=>
  (exactly odd?)                 odd?    =>
  
  (as-checker odd?)              3       =>     
  (as-checker odd?)              odd?    =not=>  
  
  (fn [actual] (= 3 actual))     3       =>
  (fn [actual] (= 3 actual))     odd?    =not=>
  )

(unfinished inner)
(defn outer [n] (inner n))

(tabular
(after-silently 
  (fact
    (outer ?actual) => "expected"
    (provided 
      (inner ?expected) => "expected"))
  (fact @reported ?arrow (one-of pass)))

?expected                      ?actual ?arrow
odd?                           3       =not=>   ;; different
odd?                           odd?    =>       ;; different

(exactly odd?)                 3       =not=>
(exactly odd?)                 odd?    =>

(as-checker odd?)              3       =>
(as-checker odd?)              odd?    =not=>

(fn [actual] (= 3 actual))     3       =not=>  ;; different
(fn [actual] (= 3 actual))     odd?    =not=>  ;; different

(as-checker (fn [actual] (= 3 actual)))     3       =>
(as-checker (fn [actual] (= 3 actual)))     odd?    =not=>)
