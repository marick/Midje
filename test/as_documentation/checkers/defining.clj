(ns as-documentation.checkers.defining
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

                ;;; Checkers that do not take arguments

;; A checker can be any predicate you define.

(defn sequence-of-even-length? [actual-result]
  (even? (count actual-result)))

(fact [0 1 2 1 3] =not=> sequence-of-even-length?)

;; Such a checker works fine, so long as you use it on the
;; right-hand-side of an arrow. However, checkers are sometimes also
;; used in the argument list of prerequisites. So consider this
;; contrived example that does *not* work:

(unfinished is-given-sequence-cause-for-happiness?)

(defn hope-to-return-happy [sequence]
  (if (is-given-sequence-cause-for-happiness? sequence) "Yes! Happy!" :I-cannot-fulfill-my-purpose))

(silent-fact
  (hope-to-return-happy [:a :b]) => "Yes! Happy!"
  (provided
    (is-given-sequence-cause-for-happiness? sequence-of-even-length?) => true))

(note-that fact-fails, some-prerequisite-was-called-with-unexpected-arguments)

;; What you might expect is that the prerequisite will return `true`
;; for any sequence of even length. That would be consistent with
;; a `sequence-of-even-length?`'s behavior on the right-hand-side.
;; However, that behavior is confusing when people are testing
;; higher-order functions like map. When people write something like
;; this:
;;          (provided (map odd? args) => [true false]
;; ... they don't generally expect that `odd?` is checking
;; the first argument to `map`; instead, they think `odd?` *is*
;; the first argument to map. And, indeed, that's how functions
;; work on the left-hand-side of an arrow.

;; To get a function to behave as a checker in prerequisites, you
;; need to use it specially:

(fact
  (hope-to-return-happy [:a :b]) => "Yes! Happy!"
  (provided
    (is-given-sequence-cause-for-happiness? (as-checker sequence-of-even-length?)) => true))

;; However, Midje already knows that the predefined checkers (like `truthy`) are
;; checkers, not functions in the code-under-test, so you don't have to declare them
;; with `as-checker`:

(fact
  (hope-to-return-happy [:a :b]) => "Yes! Happy!"
  (provided
    (is-given-sequence-cause-for-happiness? (has-prefix :a)) => true))

;; You can avoid the need for `as-checker` by defining a function as, specifically, a
;; checker:

(defchecker sequence-of-even-length? [actual-result]
  (even? (count actual-result)))

(fact
  (hope-to-return-happy [:a :b]) => "Yes! Happy!"
  (provided
    (is-given-sequence-cause-for-happiness? sequence-of-even-length?) => true))


                ;;; Checkers that do take arguments

;; Consider a checker that requires a collection to have only
;; certain elements. That's defined like this:

(defchecker only-these [& expected-elements]
  (checker [actual]
           (every? (set expected-elements) actual)))

;; This is a function that generates a checker function. The `checker`
;; on the second line is just like `fn` but additionally marks the
;; generated function as a checker, so that it can be used in
;; prerequisite argument lists.

(fact
  (hope-to-return-happy [3 1 2]) => "Yes! Happy!"
  (provided
    (is-given-sequence-cause-for-happiness? (only-these 1 2 3)) => true))



                ;;; Chatty checkers

;; Suppose your function-under-test produces a map that must contain
;; the key `:foo` and must have the value of the key `:bar` be a
;; string with a given number of characters. You could easily check
;; one call of that function like this:

(defn function-under-test []
  {:foo 1, :bar "foo"})

(fact
  (let [actual (function-under-test)]
    (contains? actual :foo) => truthy
    (:bar actual) => string?
    (count (:bar actual)) => 3))


;; Note: the following examples are not good ones for real use,
;; but they are easy to understand. See "compound checkers" below.

;; But suppose these checks were ones you'd want to apply to many
;; different calls of many different functions, not just this
;; one. It'd be tedious and perhaps misleading to repeat the three
;; checks over and over. So you might produce a checker:

(defchecker foobared [expected-count]
  (checker [actual-map]
    (and (contains? actual-map :foo)
         (string? (:bar actual-map))
         (= (count (:bar actual-map)) expected-count))))

(silent-fact
  (function-under-test) => (foobared 30000))
(note-that fact-fails)

;; This fails with this message (as of April 2012):
;;
;;   FAIL at (t_defining_checkers.clj:105)
;;   Actual result did not agree with the checking function.
;;           Actual result: {:foo 1, :bar "foo"}
;;       Checking function: (foobared 30000)
(note-that (fact-expected '(foobared 30000)))
;;
;; That's not so helpful. You'd rather it told you *which* clause of
;; the `and` failed. You can accomplish that with a one-token change
;; to the definition:

(defchecker foobared [expected-count]
  (chatty-checker [actual-map]         ; <<== on this line
    (and (contains? actual-map :foo)
         (string? (:bar actual-map))
         (= (count (:bar actual-map)) expected-count))))

;; Running this function appends some "chattier" output:
;;
;;   FAIL at (t_defining_checkers.clj:129)
;;   Actual result did not agree with the checking function.
;;           Actual result: {:foo 1, :bar "foo"}
;;       Checking function: (foobared 30000)
;;       During checking, these intermediate values were seen:
;;          (contains? actual-map :foo) => true
;;          (string? (:bar actual-map)) => true
;;          (= (count (:bar actual-map)) expected-count) => false

(silent-fact
  (function-under-test) => (foobared 30000))
(note-that fact-fails
           (fact-gave-intermediate-result (contains? actual-map :foo) => true)
           (fact-gave-intermediate-result (string? (:bar actual-map)) => true)
           (fact-gave-intermediate-result (= (count (:bar actual-map)) expected-count) => false))




;; If you want a simpler checker that doesn't take arguments, just use
;; `chatty-checker` with `def`:

(def one-or-two
     (chatty-checker [actual]
       (or (= actual 1)
           (= actual 2))))

(silent-fact
   3 => one-or-two)
;;    FAIL at (t_defining_checkers.clj:155)
;;    Actual result did not agree with the checking function.
;;            Actual result: 3
;;        Checking function: one-or-two
;;        During checking, these intermediate values were seen:
;;           (= actual 1) => false
;;           (= actual 2) => false
(note-that fact-fails
           (fact-gave-intermediate-result (= actual 1) => false)
           (fact-gave-intermediate-result (= actual 2) => false))


;;; Compound checkers
;;
;; In truth, you probably wouldn't use chatty checkers for boolean
;; expressions, because chatty checkers don't stop after the first failure,
;; which is usually desirable. Instead, you'd use `every-checker` or `some-checker`.

(fact 4 => (some-checker even? odd?))

(silent-fact 4 => (every-checker odd? (roughly 3)))
;; FAIL at (t_defining_checkers.clj:193)
;; Actual result did not agree with the checking function.
;;         Actual result: 4
;;     Checking function: (every-checker odd? (roughly 3))
;;     During checking, these intermediate values were seen:
;;        odd? => false
(note-that fact-fails
           (fact-gave-intermediate-result odd? => false)
           (fact-omitted-intermediate-result (roughly 3)))


