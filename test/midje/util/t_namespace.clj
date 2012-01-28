;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-namespace
  (:use [midje.util.namespace])
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util))

(fact "matches-symbols-in-semi-sweet-or-sweet-ns? accepts symbols from different midje namespaces"
  (let [values (zip/seq-zip '(m midje.semi-sweet/expect))
        m-node (zip/down values)
        expect-node (-> values zip/down zip/right)]
    (expect (matches-symbols-in-semi-sweet-or-sweet-ns? '(m) m-node) => truthy)
    (expect (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect) expect-node) => truthy)
    (expect (matches-symbols-in-semi-sweet-or-sweet-ns? '(n) m-node) => falsey)))

(fact "can identify semi-sweet keywords"
  (doseq [skippable '(expect midje.semi-sweet/expect
                       fake midje.semi-sweet/fake
                       data-fake midje.semi-sweet/data-fake)]
    (let [z (zip/seq-zip `(111 (~skippable 1 2 '(3)) "next"))
          skippable (-> z zip/down zip/next zip/down)]
      skippable => is-semi-sweet-keyword?)))

