(ns midje.checkers.t-combining
  (:use midje.sweet
        clojure.pprint
        [midje.checkers.defining :only [checker?]]
        midje.checkers.extended-falsehood
        midje.test-util))

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
                               [['(fn [actual] (= (str actual) "5" )) false]]})
    ))

(def mychatty (chatty-checker [actual] (or (= actual 88) (= actual 99))))

(fact "chatty checkers can be wrapped in every-checker"
  ;; But their chatty results are not propagated.
  (let [checker (every-checker mychatty)]
    (data-laden-falsehood-to-map (checker 3)) => {:actual 3
                                                  :intermediate-results
                                                  [['mychatty false]]}))

(fact "the empty every-checker passes"
  5 => (every-checker))

(def hit-count (atom 0))
(fact "the first failure short-circuits the rest"
  5 =not=> (every-checker even?
                          (fn [_] (swap! hit-count inc)))
  @hit-count => 0)
  

(facts "about checker combinators" 
  (some-checker truthy falsey)  => checker?
  (every-checker truthy falsey) => checker?  
  
  3 =>     (every-checker truthy number?)
  3 =not=> (every-checker truthy falsey)
  3 =>     (some-checker truthy falsey)
  3 =not=> (some-checker falsey string?)
  
  "works with chatty checkers' data-laden-falsehoods"
  {:a 1} =not=> (every-checker (contains {:b 1}))
  {:a 1} =not=> (some-checker  (contains {:b 1})))
