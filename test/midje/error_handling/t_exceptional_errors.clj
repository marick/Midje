;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-exceptional-errors
  (:use [midje sweet test-util]))

(after-silently 
 (fact =>)
 (fact @reported => (just (contains {:type :exceptional-user-error
                                     :macro-form '(fact =>)}))))
