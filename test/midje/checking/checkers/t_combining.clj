(ns midje.checking.checkers.t-combining
  (:require [midje.sweet :refer :all]
            [midje.checking.core :refer :all]
            [midje.checking.checkers.defining :refer [checker?]]
            [midje.test-util :refer :all]
            [such.types :as types]))

(fact "about 'every' combinations"
  (let [checker (every-checker odd?
                               (roughly 5 3)
                               (fn [actual] (= (str actual) "5" )))]

    checker => checker?

    (checker 5) => truthy
    (checker 400) => falsey  ; not odd
    (checker 99) => falsey   ; not close to 5
    (checker 3) => falsey    ; not "5"

    5 => checker
    400 =not=> checker
    99 =not=> checker
    3 =not=> checker))


;; every-checker
(facts "about form of the failure value"
  (let [sanitized (fn [actual]
                    (data-laden-falsehood-to-map
                     ( (every-checker odd?
                                      (roughly 5 3)
                                      (fn [actual] (= (str actual) "5" )))
                       actual)))]
    "In case of error, the actual value is reported"
    (sanitized 400) => (contains {:actual 400})

    (sanitized 400) => (contains {:intermediate-results
                                [['odd? false]]})
    (sanitized 99) => (contains {:intermediate-results
                               [['(roughly 5 3) false]]})
    (sanitized 3) => (contains {:intermediate-results
                               [['(fn [actual] (= (str actual) "5" )) false]]})))

(def mychatty (chatty-checker [actual] (or (= actual 88) (= actual 99))))

(fact "chatty checkers can be wrapped in every-checker"
  ;; But their chatty results are not propagated.
  (let [checker (every-checker mychatty)]
    (data-laden-falsehood-to-map (checker 3)) => {:actual 3
                                                  :intermediate-results
                                                  [['mychatty false]]})
  "containers are another form of chatty checker"
  {:a 1} =not=> (every-checker (contains {:b 1})))

(fact "checkers can be any right-hand side as well as functions"
  (let [checks-out (every-checker (fn [actual] (= (count actual) 4))
                                  #"\d+"
                                  #"5")]
    "x5xx" => checks-out
    "x1xx" =not=> checks-out
    [1 2 3 4] =not=> checks-out)
  (fact "even such a silly case as a regexp compared to a regexp"
    #"12*" => (every-checker types/regex? #"12*")))
(fact "You can even use explicit values"
  5 => (every-checker 5 odd? (roughly 5)))


(fact "the empty every-checker passes"
  5 => (every-checker))

(def hit-count (atom 0))
(fact "the first failure short-circuits the rest"
  5 =not=> (every-checker even?
                          (fn [_] (swap! hit-count inc)))
  @hit-count => 0)


;; some-checker


(facts "about some-checker"
  (some-checker truthy falsey)  => checker?
  3 =>     (some-checker truthy falsey)
  3 =not=> (some-checker falsey string?)
  {:a 1} =not=> (some-checker  (contains {:b 1})))

(facts "about form of the failure value"
  (let [checker (some-checker odd?
                              (roughly 5 3)
                              (fn [actual] (= (str actual) "6" )))]
    (checker 400) => false
    (checker 501) => true
    (checker 4) => true
    (checker 6) => true))

(fact "chatty checkers can be wrapped in some-checker"
  (let [checker (some-checker (chatty-checker [actual]
                                 (or (= actual 88) (= actual 99)))
                              even?)]
    (checker 2) => true
    (checker 88) => true
    (checker 3) => false))

(fact "checkers can be any right-hand side as well as functions"
  (let [checks-out (some-checker (fn [actual] (= (count actual) 4))
                                  #"\d+"
                                  #"5")]
    "x5xx" => checks-out
    "xax" =not=> checks-out
    [1 2 3 4] => checks-out)
  (fact "even such a silly case as a regexp compared to a regexp"
    #"12*" => (some-checker string? #"12*")))

(fact "the empty some-checker is false"
  5 =not=> (some-checker))

(def hit-count (atom 0))
(fact "the first success short-circuits the rest"
  4 => (some-checker even?
                     (fn [_] (swap! hit-count inc)))
  @hit-count => 0)
