(ns midje.error-handling.t-exceptional-errors
  (:use [midje sweet test-util]))

(capturing-failure-output
 (fact "description" (cons) =>)
 (fact @fact-output => #"Midje caught an exception when translating this form"))

(capturing-failure-output ;; Reports on top-level fact
 (fact "fine"
   (fact "description" (cons) =>))
 (fact @fact-output => #"(?s)Midje caught an exception when translating this form.*fact.*fine"))
