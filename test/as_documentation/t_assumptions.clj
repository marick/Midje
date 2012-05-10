(ns as-documentation.t-assumptions
  (:use midje.sweet
        midje.test-util))

                ;;; Checkers that do not take arguments

;; A checker can be any predicate you define.

(defn even-length [actual-result]
  (even? (count actual-result)))

(fact [0 1 2 1 3] =not=> even-length)

;; Such a checker works fine, so long as you use it on the
;; right-hand-side of an arrow. However, checkers are sometimes also
;; used in the argument list of prerequisites. So consider this
;; contrived example

(unfinished utility-function)

(defn function-under-test [sequence]
  (if (utility-function sequence) 1 2))


(run-silently 
 (fact
   (function-under-test [:a :b]) => 2
   (provided
     (utility-function even-length) => false)))

(fact @reported => has-bad-result)

;; The provided statement does NOT fake out the value of a call to
;; `utility-function` that gives it an even-length sequence. Instead,
;; it fakes out a call to `utility-function` that gives it the
;; **function** even-length. This behavior has proven to be less
;; confusing to people testing functions that take functions as
;; arguments.
;;
;; To get `even-length` to behave as a checker in prerequisites, you
;; need to define it specially:

(defchecker even-length [actual-result]
  (even? (count actual-result)))

(run-silently 
 (fact
   (function-under-test [:a :b]) => 2
   (provided
     (utility-function even-length) => false))
)
(fact @reported => passes)

 
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

(run-silently
 (fact
   [1 2 3] => (only-these 1 2 3)
   [1 2 3] => (only-these 1))
)
(fact @reported => (just [pass checker-fails]))


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

;; But suppose these checks were ones you'd want to apply to many
;; different calls of many different functions, not just this
;; one. It'd be tedious and perhaps misleading to repeat the three
;; checks over and over. So you might produce a checker:

(defchecker foobared [expected-count]
  (checker [actual-map]
    (and (contains? actual-map :foo)
         (string? (:bar actual-map))
         (= (count (:bar actual-map)) expected-count))))

(run-silently 
 (fact
   (function-under-test) => (foobared 30000))
)
(fact @reported => (just [checker-fails]))

;; This fails with this message (as of April 2012):
;;   
;;   FAIL at (t_defining_checkers.clj:105)
;;   Actual result did not agree with the checking function.
;;           Actual result: {:foo 1, :bar "foo"}
;;       Checking function: (foobared 30000)
;;
;; That's not so helpful. You'd rather it told you *which* clause of
;; the `and` failed. You can accomplish that with a one-token change
;; to the definition:
;;

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

(run-silently 
 (fact
   (function-under-test) => (foobared 30000))
)
(fact @reported => (just [checker-fails]))

;; If you want a simpler checker that doesn't take arguments, just use
;; `chatty-checker` with `def`:

(def one-or-two
     (chatty-checker [actual]
       (or (= actual 1)
           (= actual 2))))

(run-silently
 (fact
   3 => one-or-two)
 )

;;    FAIL at (t_defining_checkers.clj:155)
;;    Actual result did not agree with the checking function.
;;            Actual result: 3
;;        Checking function: one-or-two
;;        During checking, these intermediate values were seen:
;;           (= actual 1) => false
;;           (= actual 2) => false

(fact @reported => (just [checker-fails]))
