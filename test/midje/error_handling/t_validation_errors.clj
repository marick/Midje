;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-validation-errors
  (:use [midje sweet test-util]
        [midje.error-handling.validation-errors]
        [midje.internal-ideas.file-position :only [form-position]]
        [clojure.algo.monads]
        midje.util))

(expose-testables midje.error-handling.validation-errors)

(fact "any form can be turned into a validation-error form"
  (meta (as-validation-error '(form))) => (contains {:midje-validation-error true})
  (as-validation-error '(form)) => validation-error-form?)

(def my-favorite-error-form (as-validation-error '(error form)))

(fact "there is an error monad for Midje"
  (domonad midje-maybe-m
           [a 1
            b (inc a)]
           b) => 2

  (let [result (domonad midje-maybe-m
                        [a my-favorite-error-form
                         b (inc a)]
                        b)]
    result => my-favorite-error-form
    result => validation-error-form?))

(fact "there is syntactic sugar for it"
  (valid-let [a my-favorite-error-form
              b (inc a)]
    b) => my-favorite-error-form)

(fact "errors can spread to infect whole collections"
  (spread-validation-error [1 2 3]) => '(1 2 3)
  (spread-validation-error [1 my-favorite-error-form]) => my-favorite-error-form)

(fact "you can insist a collection of items be fully valid"
  (let [suspect [1 2 3]]
    (with-valid suspect (second suspect)) => 2)
  (let [suspect [1 (as-validation-error '(str "this would report an error"))]]
    (with-valid suspect "this is the wrong return value") => "this would report an error"))
    


(fact "there is a helper function that produces error-reporting forms"
  (report-validation-error '(anything) "note 1" "note 2")
  => '(clojure.test/report {:type :validation-error
                            :notes '["note 1" "note 2"]
                            :position '...form-position... })
  (provided
    (form-position '(anything)) => ...form-position...)

  (report-validation-error '(whatever)) => validation-error-form?)

(fact "can produce a basic error-reporting form, w/ form always as final note"
  (simple-report-validation-error '(anything) "note 1" "note 2")
  => '(clojure.test/report {:type :validation-error
                            :notes '["note 1" "note 2" "(anything)"]
                            :position '...form-position... })
  (provided
    (form-position '(anything)) => ...form-position...)

  (simple-report-validation-error '(whatever)) => validation-error-form?)

