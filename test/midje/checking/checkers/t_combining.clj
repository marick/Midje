(ns midje.checking.checkers.t-combining
  (:use midje.sweet
        midje.checking.core
        [midje.checking.checkers.defining :only [checker?]]
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
                               [['(fn [actual] (= (str actual) "5" )) false]]})
    ))

(def mychatty (chatty-checker [actual] (or (= actual 88) (= actual 99))))

(fact "chatty checkers can be wrapped in every-checker"
  ;; But their chatty results are not propagated.
  (let [checker (every-checker mychatty)]
    (data-laden-falsehood-to-map (checker 3)) => {:actual 3
                                                  :intermediate-results
                                                  [['mychatty false]]})
  "containers are another form of chatty checker"
  {:a 1} =not=> (every-checker (contains {:b 1})))

(fact "checkers can be regexs as well as functions"
  (let [checker (every-checker (fn [actual] (= (count actual) 1))
                               #"/d+"
                               #"5")]

    (checker "5") => truthy
    (checker "1") => falsey))

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

(fact "checkers can be regexs as well as functions"
  (let [checker (some-checker (fn [actual] (= (count actual) 1))
                               #"\d+")]

    (checker "11") => truthy
    (checker "test") => falsey))

(fact "the empty some-checker false"
  5 =not=> (some-checker))

(def hit-count (atom 0))
(fact "the first success short-circuits the rest"
  4 => (some-checker even?
                     (fn [_] (swap! hit-count inc)))
  @hit-count => 0)


