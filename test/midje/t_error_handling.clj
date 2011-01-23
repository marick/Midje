;; -*- indent-tabs-mode: nil -*-

(ns midje.t-error-handling
  (:use [midje sweet error-handling test-util]))

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
 (fact @reported => (just (contains {:type :user-error }))))
 
