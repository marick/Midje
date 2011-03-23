;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-recognizing
  (:use [midje.midje-forms.recognizing])
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(fact "namespacey-match accepts symbols from different midje namespaces"
  (let [values (zip/seq-zip '(m midje.semi-sweet/expect))
        m-node (zip/down values)
        expect-node (-> values zip/down zip/right)]
    (expect (namespacey-match '(m) m-node) => truthy)
    (expect (namespacey-match '(expect) expect-node) => truthy)
    (expect (namespacey-match '(n) m-node) => falsey)))

(fact "an embedded expect form can be recognized"
  (zip/seq-zip '(expect x => y)) => loc-is-at-full-expect-form?
  (zip/seq-zip '(midje.semi-sweet/expect x => y)) => loc-is-at-full-expect-form?
  (zip/seq-zip '(+ x y)) =not=> loc-is-at-full-expect-form?
  (zip/seq-zip 'expect) =not=> loc-is-at-full-expect-form?)

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => loc-is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => loc-is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) =not=> loc-is-head-of-form-providing-prerequisites?))

(fact "can identify and skip over semi-sweet keywords (currently 'expect' and 'fake')"
  (doseq [skippable '(expect fake midje.semi-sweet/expect midje.semi-sweet/fake)]
    (let [z (zip/seq-zip `(111 (~skippable 1 2 '(3)) "next"))
          skippable (-> z zip/down zip/next zip/down)]
      skippable => loc-is-semi-sweet-keyword?)))

(fact "can ask if at first element of X =?> Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
    (possible '( (f 1) )) =not=> loc-is-start-of-check-sequence?
    (possible '( (f 1) (f 2))) =not=> loc-is-start-of-check-sequence?

    (possible '( (f 1) => 2)) => loc-is-start-of-check-sequence?
    (possible '( (f 1) =not=> 2)) => loc-is-start-of-check-sequence?
    (possible '( (f 1) => 2 :key 'value)) => loc-is-start-of-check-sequence?
    (possible '( (f 1) midje.semi-sweet/=> 2)) => loc-is-start-of-check-sequence?))

(facts "recognizing setup/teardown forms"
  '[ (before :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks) ... ] =not=>  seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after (- 2 2)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after ) ... ] =not=> seq-headed-by-setup-teardown-form?

  '[ (after :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (around :checks (let [x 1] ?form)) ... ] => seq-headed-by-setup-teardown-form?)

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

;; Folded prerequisites

(defmacro some-macro [& rest] )

   
(fact "a fake that needs unfolding has a nested left-hand-side"
  "things not a proper fake macro"
  '1                                            =not=> fake-that-needs-unfolding?
  '()                                           =not=> fake-that-needs-unfolding?
  '(fake (f (h 1)))                             =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/non-fake (f (h 1)))        =not=> fake-that-needs-unfolding?

  "Things that might be misinterpreted as nested funcalls"
  '(midje.semi-sweet/fake (f) =test=> 3)        =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1) =test=> 3)      =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1 '(foo)) =test=> 3) =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1 [foo]) =test=> 3) =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1 {foo 1}) =test=> 3) =not=> fake-that-needs-unfolding?

  "These are real nested function calls"
  '(midje.semi-sweet/fake (f (h 1)) =test=> 3)  => fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1 (h 1)) =test= 3) => fake-that-needs-unfolding?

  "but don't decide to unfold a checker used as argument matcher"
  '(midje.semi-sweet/fake (f 1 (exactly even?)) =test=> 3) =not=> fake-that-needs-unfolding?
  "Sad but true: a cons is not a list."
  (cons 'midje.semi-sweet/fake '((f (h 3)) =test=> 3))
  => fake-that-needs-unfolding?

  "Macros are surprisingly hard to get right"
;  '(midje.semi-sweet/fake (f 1 (some-macro 33)) =test=> 3) =not=> fake-that-needs-unfolding?
  )
