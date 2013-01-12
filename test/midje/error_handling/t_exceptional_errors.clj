(ns midje.error-handling.t-exceptional-errors
  (:use [midje sweet test-util]))

(capturing-output
 (fact "description" (cons) =>)
 (fact @test-output => #"Midje caught an exception when translating this form"))

(capturing-output ;; Reports on top-level fact
 (fact "fine"
   (fact "description" (cons) =>))
 (fact @test-output => #"(?s)Midje caught an exception when translating this form.*fact.*fine"))
