;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-monadic
  
  (:use [midje sweet test-util]
        [midje.error-handling.monadic]
        [midje.internal-ideas.file-position :only [form-position]]
        [clojure.contrib monads]))

(fact "any form can be turned into a user-error form"
  (meta (as-user-error '(form))) => (contains {:midje-user-error true})
  (as-user-error '(form)) => user-error-form?)

(def my-favorite-error-form (as-user-error '(error form)))

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
    result => user-error-form?))

(fact "there is syntactic sugar for it"
  (error-let [a my-favorite-error-form
              b (inc a)]
    b) => my-favorite-error-form)

(fact "safely turns a function application into one that propagates errors."
  (safely concat my-favorite-error-form '()) => my-favorite-error-form)

(fact "errors can spread to infect whole collections"
  (spread-error [1 2 3]) => '(1 2 3)
  (spread-error [1 my-favorite-error-form]) => my-favorite-error-form)

(fact "you can insist a collection of items be fully valid"
  (let [suspect [1 2 3]]
    (with-valid suspect (second suspect)) => 2)
  (let [suspect [1 (as-user-error '(str "this would report an error"))]]
    (with-valid suspect "this is the wrong return value") => "this would report an error"))
    


(fact "there is a helper function that produces error-reporting forms"
  (user-error-report-form '(anything) "note 1" "note 2")
  => '(clojure.test/report {:type :user-error
                            :notes '["note 1" "note 2"]
                            :position '...form-position... })
  (provided
    (form-position '(anything)) => ...form-position...)

  (user-error-report-form '(whatever)) => user-error-form?)


