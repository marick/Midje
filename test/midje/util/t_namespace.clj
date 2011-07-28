;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-namespace
  (:use [midje.util.namespace])
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

