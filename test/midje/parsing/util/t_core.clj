(ns midje.parsing.util.t-core
  (:use midje.parsing.util.core
        midje.sweet
        midje.test-util)
  (:require [clojure.zip :as zip]))

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
      skippable => semi-sweet-keyword?)))

(fact "stringlike-matches?"
  (stringlike-matches? "foo" "ofoop") => true
  (stringlike-matches? "foo" "ooop") => false
  (stringlike-matches? "foo" nil) => false
  (stringlike-matches? "foo" [1 2 3]) => false
  (stringlike-matches? #"fo." "ofop") => true
  (stringlike-matches? #"fo." "ooop") => false
  (stringlike-matches? #"fo." false) => false)

(facts "a form's reader-assigned line-number can be extracted"
  (reader-line-number (with-meta '(fact (this that)) {:line 23})) => 23
  "or, failing that: try top-level subforms"
  (reader-line-number `(fact
                         (+ 1 2)
                         ~(with-meta '(this that) {:line 23})
                         ~(with-meta '(this that) {:line 22223}))) => 23
  "or a default value"
  (reader-line-number (with-meta '(fact "text") {})) => "0 (no line info)")


(fact "can unquote a form"
  (dequote '1) => 1
  (dequote 1) => 1
  (dequote '(some form)) => '(some form))

  
