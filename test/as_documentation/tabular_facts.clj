(ns as-documentation.tabular-facts
  (:require [midje.repl :refer :all]
            [midje.test-util :refer :all]))

;;; When you have many similar tests, you can use *tabular facts* to
;;; clarify what's special about each test. Here, for example, is a tabular
;;; fact about addition:

(tabular "tabular facts can take a doc string"
 (fact (+ ?a ?b) => ?result )
 ?a    ?b      ?result
 1     2       3
 1     0       1)

;;; You can use the repl tools to work with tabular facts just as you
;;; can any other top-level fact:

(fact
  (map fact-description (fetch-facts *ns* "tabular facts can take a doc string"))
  => ["tabular facts can take a doc string"])

;;; On failure, the line number points to the fact, but the message
;;; is annotated with information about the substitutions.

(capturing-failure-output
 (tabular
   (fact (+ ?a ?b) => ?result )
   ?a    ?b      ?result
   1     2       3333
   1     0       11)
 (fact
   @fact-output => (contains "With table substitutions: [?a 1")
   @fact-output => (contains "                           ?b 2")
   @fact-output => (contains "                           ?result 3333]")
   (strip-ansi-coloring @fact-output) => (contains "Expected:\n3333")
   (strip-ansi-coloring @fact-output) => (contains "Actual:\n3")
   @fact-output => (contains "With table substitutions: [?a 1")
   @fact-output => (contains "                           ?b 0")
   @fact-output => (contains "                           ?result 11]")
   (strip-ansi-coloring @fact-output) => (contains "Expected:\n11")
   (strip-ansi-coloring @fact-output) => (contains "Actual:\n1")))


;;;                                     More about doc Strings and Metadata

;;; If you prefer, you can put the doc string on the enclosed fact rather than the
;;; tabular fact:

(tabular
  (fact "Put the doc string wherever you prefer"
    (+ ?a ?b) => ?result )
  ?a    ?b      ?result
  1     2       3
  1     0       1)

;;; You can add other metadata to tabular facts, and select ones to check with it:

(tabular
  (fact :a-tabular-fact
    (+ ?a ?b) => ?result )
  ?a    ?b      ?result
  1     2       3
  1     0       1)

(fact
  (let [marked-facts (fetch-facts *ns* :a-tabular-fact)]
    (count marked-facts) => 1
    (:a-tabular-fact (meta (first marked-facts))) => true))


;;;                                     Miscellany

(capturing-failure-output
 (fact "you can nest tabular facts within other facts"
   (+ 1 1) => 2
   (tabular
     (fact (+ ?a ?b) => 1)
     ?a ?b
     1   0
     0   1
     1   1))
 (fact
   @fact-output => (contains "With table substitutions: [?a 1")
   @fact-output => (contains "                           ?b 1]")
   (strip-ansi-coloring @fact-output) => (contains "Expected:\n1")
   (strip-ansi-coloring @fact-output) => (contains "Actual:\n2")))

;; It's natural to think of substituting values and expressions,
;; but you can substitute anything, such as Midje arrows.
(tabular
  (fact (+ ?a ?b) ?arrow ?expected)
  ?a ?b ?arrow ?expected
  1  1  =>     2
  1  2  =not=> 3000)
