;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-validation-errors
  (:use [midje sweet test-util]
        [midje.error-handling.validation-errors]
        [midje.internal-ideas.file-position :only [form-position]]
        [clojure.algo.monads]
        midje.util))

(expose-testables midje.error-handling.validation-errors)

(fact "any form can be turned into a validation-error form"
  (meta (as-validation-error '(form))) => (contains {:midje/syntax-validation-error true})
  (as-validation-error '(form)) => validation-error-form?)

(def my-valid-form '(expect 1 => 1))
(def my-favorite-error-form (as-validation-error '(error form)))

(fact "there is a validation monad for Midje"
  (domonad syntax-validate-m
           [a 1
            b (inc a)]
         b) => 2

  (let [result (domonad syntax-validate-m
                        [a my-favorite-error-form
                         b (inc a)]
                        b)]
    result => my-favorite-error-form
    result => validation-error-form?))

(fact "there is a helper function that produces error-reporting forms"
  (validation-error-report-form '(anything) "note 1" "note 2")
  => '(clojure.test/report {:type :validation-error
                            :notes '["note 1" "note 2"]
                            :position '...form-position... })
  (provided
    (form-position '(anything)) => ...form-position...)

  (validation-error-report-form '(whatever)) => validation-error-form?)

(fact "can produce a basic error-reporting form, w/ form always as final note"
  (simple-validation-error-report-form '(anything) "note 1" "note 2")
  => '(clojure.test/report {:type :validation-error
                            :notes '["note 1" "note 2" "(anything)"]
                            :position '...form-position... })
  (provided
    (form-position '(anything)) => ...form-position...)

  (simple-validation-error-report-form '(whatever)) => validation-error-form?)

