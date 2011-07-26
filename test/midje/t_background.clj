;; -*- indent-tabs-mode: nil -*-

(ns midje.t-background
  (:require [clojure.zip :as zip])
  (:use [midje sweet test-util]
        [midje.util.wrapping :only [for-wrapping-target?]]
        [midje.util unify]
        [midje.background :only [separate-background-forms setup-teardown-bindings
                                 seq-headed-by-setup-teardown-form? background-wrappers]]))
(testable-privates midje.background
                   prerequisites-to-fakes state-wrapper)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])



(tabular
 (fact "separate-background-forms divides forms into background and other things"
   (separate-background-forms '?in) => '[ ?background-forms  ?other-forms])

 ?in                                    ?background-forms               ?other-forms
 [ (against-background ..back1..
                       ..back2..)
   ..claim1..
   ..claim2..]                         [..back1.. ..back2..]          [..claim1.. ..claim2..]

 [ (against-background ..back1..)
   ..claim1..
   (against-background ..back2..)]      [..back1.. ..back2..]         [..claim1..] 

 []                                     []                              []
 [ (f 1) => 3 ]                         []                              [ (f 1) => 3 ]

 [(against-background)
  (f 1) => 3 ]                          []                              [(f 1) => 3]
)


(facts "dissecting setup/teardown forms"
  (setup-teardown-bindings '(before :checks (+ 1 1))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1), ?after nil})

  (setup-teardown-bindings '(before :checks (+ 1 1) :after (- 2 2))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1),
                ?after :after, ?second-form (- 2 2)})

  (setup-teardown-bindings '(after :checks (+ 1 1))) =>
    (contains '{?key after, ?when :checks, ?first-form (+ 1 1)})

  (setup-teardown-bindings '(around :checks (let [x 1] ?form))) =>
    (contains '{?key around, ?when :checks,
                ?first-form (let [x 1] ?form) }))

(facts "recognizing setup/teardown forms"
  '[ (before :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks) ... ] =not=>  seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after (- 2 2)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after ) ... ] =not=> seq-headed-by-setup-teardown-form?

  '[ (after :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (around :checks (let [x 1] ?form)) ... ] => seq-headed-by-setup-teardown-form?)




;; wrapping



(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (prerequisites-to-fakes []) => []
  (prerequisites-to-fakes '[(f 1) => 2]) =>
                                 '[(midje.semi-sweet/fake (f 1) => 2 :type :background)]
  (prerequisites-to-fakes '[   (f 1) => 2 :foo 'bar (f 2) => 33 ]) => 
                              '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar :type :background)
                                (midje.semi-sweet/fake (f 2) => 33 :type :background) ]

  "other types are left alone"
  (prerequisites-to-fakes
   '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checks (swap! test-atom (constantly 0))) ]

 "mixtures"
 (prerequisites-to-fakes
   '[ (f 1) => 2 (before :checks (swap! test-atom (constantly 0))) (f 2) => 3 ]) =>
   '[ (midje.semi-sweet/fake (f 1) => 2 :type :background)
      (before :checks (swap! test-atom (constantly 0)))
      (midje.semi-sweet/fake (f 2) => 3 :type :background) ]
 
 "error cases"
 (prerequisites-to-fakes '[ (after anything) ]) => (throws Error)
 )

(defn guard-special-form [bindings]
  (assoc (dissoc bindings '?danger) '?danger (str (bindings '?danger))))

(defmacro wrapping-form-is [ original expected ]
  (let [bindings (unify expected (state-wrapper original)) ]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" }))

;; The magical symbol that's used in wrapper substitution can't be used in
;; a fact because it gets substituted. So we let the caller use "danger" instead.
(defn- form-matching? [expected]
  (fn [actual] (= actual
                  (subst expected {'?danger 'midje.t-background/?form}))))

(fact "canonicalized setup/teardown wrappers can be put into final form"
  (let [final (state-wrapper '(before :checks (do-something)))]
    final => (form-matching? '(try (do-something) ?danger (finally nil)))
    final => (for-wrapping-target? :checks))

  (let [final (state-wrapper '(before :facts (do-something) :after (finish)))]
    final => (form-matching? '(try (do-something) ?danger (finally (finish))))
    final => (for-wrapping-target? :facts))

  (let [final (state-wrapper '(after :all (do-something)))]
    final => (form-matching? '(try ?danger (finally (do-something))))
    final => (for-wrapping-target? :all))

  (let [final (state-wrapper '(around :checks (let [x 1] ?form)))]
    final => (form-matching? '(let [x 1] ?danger))
    final => (for-wrapping-target? :checks))
)

(facts "about safe expansion of weird forms"
  (map? {1 'do}) => truthy
  (first (second '(midje.semi-sweet.expect (midje.sweet.fact 1 => 2)))) => 'midje.sweet.fact
  (set? #{1 'do}) => truthy)


