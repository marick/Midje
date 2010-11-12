(ns midje.sweet.t-sweet-to-semi-sweet-rewrite-test
  (:use [midje.util.forms] :reload-all)
  (:use clojure.test)
  (:use midje.test-util)
)

(deftest should-accept-symbols-in-more-than-one-namespace
  (let [values (zip/seq-zip '(m midje.semi-sweet/expect))
	m-node (zip/down values)
	expect-node (-> values zip/down zip/right)]
    (expect (namespacey-match '(m) m-node) => truthy)
    (expect (namespacey-match '(expect) expect-node) => truthy)
    (expect (namespacey-match '(n) m-node) => falsey)))

