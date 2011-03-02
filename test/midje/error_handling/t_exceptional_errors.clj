;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-exceptional-errors
  (:use [midje sweet test-util]))

(future-fact "Find case where fact blows up with exceptional error"

(after-silently 
 (fact (f) =>)
 (fact @reported => (just (contains {:type :exceptional-user-error }))))
 
)
