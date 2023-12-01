(ns midje.parsing.2-to-lexical-maps.t-fakes
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.parsing.2-to-lexical-maps.fakes :refer :all]
            [midje.util :refer :all]))

(silent-fact "check line number and file position of t_fakes errors"
  (f) => 0
  (provided
    (deref cons) => 0))  ; for example
(note-that (failure-was-in-file "t_fakes.clj")
           (failure-was-at-line 10))

(defn foo [x] (set [1]))
(silent-fact "Functions, like `set`, used deep in faking logic can't be faked themselves"
  (foo 1) => :what
  (provided
    (set [1]) => true))
(note-that (fact-captured-throwable-with-message #"You seem to have created a prerequisite*"))


(defn noop [y] y)
(defn my-fn [x] (noop (str x)))

(silent-fact "results in stack overflow"
  (my-fn 'url) => 'some-result
  (provided (noop (my-fn 'url)) => 'some-result))
(note-that (fact-captured-throwable-with-message #"You seem to have created a prerequisite*"))

(fact "note that you can get around it by not mocking `str`"
  (let [s (str 'url)]
    (my-fn 'url) => 'some-result
    (provided (noop s) => 'some-result)))
