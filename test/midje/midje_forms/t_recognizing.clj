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

(facts "recognizing setup/teardown forms"
  (seq-headed-by-setup-teardown-form? '[ (before :checking (+ 1 1)) ... ]) => truthy
  (seq-headed-by-setup-teardown-form? '[ (before :checking) ... ]) => falsey

  
  (seq-headed-by-setup-teardown-form? '[ (before :checking (+ 1 1) :after (- 2 2)) ... ]) => truthy
  (seq-headed-by-setup-teardown-form? '[ (before :checking (+ 1 1) :after ) ... ]) => falsey

  (seq-headed-by-setup-teardown-form? '[ (after :checking (+ 1 1)) ... ]) => truthy

  (seq-headed-by-setup-teardown-form? '[ (around :checking (let [x 1] ?form)) ... ]) => truthy

)

(facts "dissecting setup/teardown forms"  ;; here to avoid circular dependencies
  (setup-teardown-bindings '(before :checking (+ 1 1))) =>
    (map-containing '{?key before, ?when :checking, ?first-form (+ 1 1), ?after nil})

  (setup-teardown-bindings '(before :checking (+ 1 1) :after (- 2 2))) =>
    (map-containing '{?key before, ?when :checking, ?first-form (+ 1 1),
		      ?after :after, ?second-form (- 2 2)})

  (setup-teardown-bindings '(after :checking (+ 1 1))) =>
    (map-containing '{?key after, ?when :checking, ?first-form (+ 1 1)})

  (setup-teardown-bindings '(around :checking (let [x 1] ?form))) =>
    (map-containing '{?key around, ?when :checking,
		      ?first-form (let [x 1] ?form) })
)
