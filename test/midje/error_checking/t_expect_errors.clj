;; -*- indent-tabs-mode: nil -*-

(ns midje.error-checking.t-expect-errors
  (:use [midje.sweet]
        [midje.test-util]))

;; No longer needed. Delete when error handling in good shape
;; (def error-reporter (as-user-error '(fn [] 'reporting-the-error)))
;; (expect (expect-expansion '(+ 1 1) '=> '3 [:some-kind-of-fake]) => error-reporter
;;        (fake (midje.semi-sweet/fakes-and-overrides anything) => error-reporter))

; (expect (+ 1 2) =>)

(after-silently
 (expect (+ 1 2) =>)
 (expect @reported => (just (contains {:type :user-error}))))
