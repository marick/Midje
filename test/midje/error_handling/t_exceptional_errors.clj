;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-exceptional-errors
  (:use [midje sweet test-util]))

(after-silently 
 (fact "description" =>)
 (fact @reported => (just (contains {:type :exceptional-user-error
                                     :description "description"
                                     :macro-form '(fact "description" =>)}))))

;; report ONLY top level fact's description when there's a error in nested facts

(after-silently
  (fact "fine"
    (fact "description" =>))
 (fact @reported => (just (contains {:type :exceptional-user-error
                                     :description "fine"
                                     :macro-form '(fact "fine"
                                                    (fact "description" =>))}))))
