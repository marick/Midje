(ns midje.midje-forms.t-recognizing
  (:use [midje.midje-forms.recognizing] :reload-all)
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
  (zip/seq-zip '(+ x y)) => (complement loc-is-at-full-expect-form?)
  (zip/seq-zip 'expect) => (complement loc-is-at-full-expect-form?))

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => loc-is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => loc-is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) => (complement loc-is-head-of-form-providing-prerequisites?)))

(fact "can identify and skip over semi-sweet keywords (currently 'expect' and 'fake')"
  (doseq [skippable '(expect fake midje.semi-sweet/expect midje.semi-sweet/fake)]
    (let [z (zip/seq-zip `(111 (~skippable 1 2 '(3)) "next"))
	  skippable (-> z zip/down zip/next zip/down)]
      skippable => loc-is-semi-sweet-keyword?)))

(fact "can ask if at first element of X => Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
    (possible '( (f 1) )) => (complement loc-is-start-of-arrow-sequence?)
    (possible '( (f 1) (f 2))) => (complement loc-is-start-of-arrow-sequence?)

    (possible '( (f 1) => 2)) => loc-is-start-of-arrow-sequence?
    (possible '( (f 1) => 2 :key 'value)) => loc-is-start-of-arrow-sequence?
    (possible '( (f 1) midje.semi-sweet/=> 2)) => loc-is-start-of-arrow-sequence?))

(facts "recognizing setup/teardown forms"
  '[ (before :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks) ... ] => (complement seq-headed-by-setup-teardown-form?)
  '[ (before :checks (+ 1 1) :after (- 2 2)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after ) ... ] => (complement seq-headed-by-setup-teardown-form?)

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
