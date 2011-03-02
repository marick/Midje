;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-semi-sweet-errors
  (:use [midje.sweet]
        [midje.error-handling.semi-sweet-errors]
        [midje.util.file-position :only [form-position]]
        [midje.test-util]))

(facts "about expect valid validation"
  (against-background (form-position anything) => ...position...)
  "returns tail part of structure"
  (validate-expect '(expect (f 1) => 3)) => '[(f 1) => 3]

  (println "HI")
  (let [too-short '(expect (f 1) =>)]
    (validate-expect too-short) => user-error-form?))

  

  

;;; Full-bore tests. 

(after-silently
 (expect (+ 1 2) =>)
 (expect @reported => (just (contains {:type :user-error}))))
