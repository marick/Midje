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
 (fact @reported => (just (just {:type :user-error,
                                 :position ["t_error_handling.clj", 17],
                                 :message #"must look like.*\.\.\.movie\.\.\." }))))


;; (fact (f) => 3 (provided ...movie... => (+ 1 3)))

;;Fakes

(def ...movie... :...movie...)
(let [error-regexp #"must look like.*\.\.\.movie\.\.\."
      raw-fake (fake ...movie... => 3) 
      numbered-raw-fake (fake ...movie... => 3
                              :file-position (midje.util.file-position/line-number-known 5))]
  (fact
    raw-fake => broken-fake?
    raw-fake => (contains {:message error-regexp})
    numbered-raw-fake => broken-fake?
    numbered-raw-fake => (contains {:message error-regexp})
    ))
