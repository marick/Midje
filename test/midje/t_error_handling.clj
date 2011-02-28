;; -*- indent-tabs-mode: nil -*-

(ns midje.t-error-handling
  (:use [midje sweet error-handling test-util]
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

(fact "there is a helper function that produces error-reporting forms"
  (let [result (user-error-report-form "message" "position")]
    result => '(clojure.test/report {:type :user-error
                                     :message "message"
                                     :position "position" })
    result => user-error-form?))




;;Util

(fact "recognizing broken fakes"
  1 => (complement broken-fake?)
  (make-broken-fake {} "foo") => broken-fake?)

;; User errors are reported specially.

;; Fake errors
(unfinished f)
(after-silently
 (fact (f) => 3 (provided ...movie... => (exactly odd?)))
 (fact @reported => (just (contains {:type :user-error,
                                 :message #"must look like.*\.\.\.movie\.\.\." }))))


;; (fact (f) => 3 (provided ...movie... => (+ 1 3)))

;;Fakes

(def ...movie... :...movie...)
(let [error-regexp #"must look like.*\.\.\.movie\.\.\."
      raw-fake (fake ...movie... => 3) ]
  (fact
    raw-fake => broken-fake?
    raw-fake => (contains {:message error-regexp})
    ))

(after-silently 
 (fact (f) =>)
 (fact @reported => (just (contains {:type :exceptional-user-error }))))
 
