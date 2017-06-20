(ns implementation.parsing.util.fim_exception_line_numbers
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]))

;; These tests check line numbers, which have to be carefully copied into the
;; metadata of constructed forms.

(capturing-failure-output
 (macroexpand '(fact "description" (+ 1 1) =throw-parse-exception=> 2))
 (fact
   @fact-output => #"Midje caught an exception when translating this form"
   @fact-output => #"fim_exception_line_numbers.clj:10"))


(capturing-failure-output ;; Reports on outer-level fact
 (macroexpand '(fact "fine" (fact "description" (+ 1 1) =throw-parse-exception=> 2)))
 (fact
   @fact-output => #"(?s)Midje caught an exception when translating this form.*fact.*fine"
   @fact-output => #"fim_exception_line_numbers.clj:17"))

(capturing-failure-output
 (macroexpand '(tabular (fact (?a) =throw-parse-exception=> 1) ?a 2))
 (fact
   @fact-output => #"(?s)Midje caught an exception when translating this form.*tabular.*fact"
   @fact-output => #"fim_exception_line_numbers.clj:23"))
