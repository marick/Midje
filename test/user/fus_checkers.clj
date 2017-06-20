(ns user.fus-checkers
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(fact "simple checkers are exported into midje.sweet"
  1 => truthy
  nil => falsey
  'foo => anything
  odd? => (exactly odd?)
  5 => (roughly 5)
  (throw (Error.)) => (throws Error))

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



(future-fact "Failures from chatty-checkers-within-functions propagate chatty information"

     (defn as-sets [& expected]
       (let [set-of-sets #(set (map set %))]
         (fn [actual]
           ( (just (set-of-sets expected)) (set-of-sets actual)))))


     (silent-fact
       [ [1] [2 3] ] => (as-sets [ [1] ]))

     @silent-fact:last-raw-failure => (contains {:type :actual-result-did-not-match-checker
                                                 :actual [ [1] [2 3]]
                                                 :expected-result-form '(as-sets [[1]])
                                                 :notes  ["Expected one element. There were two."]}))


;; The behavior of checkers is different in prerequisites

(tabular
  (fact ?actual ?arrow ?expected)

  ?expected                      ?actual ?arrow
  odd?                           3       =>
  odd?                           odd?    =not=>

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
  (fact
    (silent-fact
     (outer ?actual) => "expected"
     (provided
       (inner ?expected) => "expected"))
    @silent-fact:failure-count ?arrow zero?)

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
