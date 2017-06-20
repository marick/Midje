(ns behaviors.t-formulas
  (:require [midje.config :as config]
            [midje.test-util :refer :all]
            [midje.sweet :refer :all]
            [midje.util.ecosystem :refer :all]
            [midje.parsing.0-to-fact-form.formulas :refer [*num-trials* with-num-trials]]))

;;;; Formulas

;; First we create our own generator functions since Midje doesn't include any.
(defn make-string []
  (rand-nth ["a" "b" "c" "d" "e" "f" "g" "i"]))
(defn- gen-int [pred]
  (rand-nth (filter pred [-999 -100 -20 -5 -4 -3 -2 -1 0 1 2 3 4 5 20 100 999])))

;; Formulas are a generative style test macro. Each binding has a generator on
;; the right and a symbol on the left that will hold the generated value

(formula "can now use simple generative-style formulas - with multiple bindings"
  [a (make-string) b (make-string) c (make-string)]
  (str a b c) => (has-prefix (str a b)))

;; You can use provided to make fakes inside of a formula
(unfinished f)
(defn g [x] (str (f x) x))

(formula "'provided' works"
  [a (make-string)]
  (g a) => (str "foo" a)
  (provided
    (f anything) => "foo"))


;; Failed formulas report once per formula regardless how many trials were run

(silent-formula "some description" [a "y"] a => :foo)
(note-that (fails 1 time))

;; Passing formulas run the generator many times, and evaluate
;; their body many times - number of trials is rebindable
(defn-call-countable y-maker [] "y")
(defn-call-countable my-str [s] (str s))

(binding [*num-trials* 77]
  (formula [y (y-maker)]
    (my-str y) => "y"))
(fact @y-maker-count => 77)
(fact @my-str-count => 77)


;; There is syntactic sugar for binding *num-trials*
(defn-call-countable k-maker [] "k")
(with-num-trials 1000
  (formula [a 1] (k-maker) => "k")
  (formula [a 1] (k-maker) => "k")
  (formula [a 1] (k-maker) => "k"))
(fact @k-maker-count => 3000)

;; Can specify number of trials to run in options map - overrides *num-trials* var value
(defn-call-countable foo-maker [] "foo")
(defn-call-countable my-double-str [s] (str "double" s))

(binding [*num-trials* 111]  ;; this will be overridden by opt map
  (formula "asdf" {:num-trials 88} [foo (foo-maker)]
    (my-double-str foo) => "doublefoo"))
(fact @foo-maker-count => 88)
(fact @my-double-str-count => 88)


;; Runs only as few times as needed to see a failure
(defn-call-countable z-maker [] "z")
(defn-call-countable my-identity [x] (identity x))

(silent-formula [z (z-maker)]
  (my-identity z) => "clearly not 'z'")
(fact "calls generator once" @z-maker-count => 1)
(fact "evalautes body once" @my-identity-count => 1)

;;;; Other

(future-formula "demonstrating the ability to create future formulas"
  [a 1]
  a => 1)


;;;; Validation

;; The following factsexpress an assortment of ways that formulas
;; could be expressed with invalid syntax

(unfinished h)

(let [error-regexp #"There is no expection in your formula form"]
  (silent-formula [a 1])
  (silent-formula [a 1] 1)
  (silent-formula "a doc string" [a 1] (contains 3))

  (silent-formula "ignores arrows in provideds" [a 1]
    (contains 3)
    (provided (h anything) => 5))

  (silent-formula "ignores arrows in against-background" [a 1]
    (contains 3)
    (against-background (h anything) => 5))

  (silent-formula "ignores arrows in against-background - even when it comes first"
    [a 1]
    (against-background (h anything) => 5)
    (contains 3))

  (silent-formula "ignores arrows in background" [a 1]
    (contains 3)
    (background (h anything) => 5))

  (silent-formula "ignores arrows in background - even when it comes first"
    [a 1]
    (background (h anything) => 5)
    (contains 3))
  (for-each-failure (note-that parse-error-found, (fact-failed-with-note error-regexp)))
  )

(let [error-regexp  #"Formula requires bindings to be an even numbered vector of 2 or more"]
  (silent-formula "a doc string" :not-vector 1 => 1)
  (silent-formula "a doc string" [a 1 1] 1 => 1)
  (silent-formula "a doc string" [] 1 => 1)
  (silent-formula "a doc string" {:num-trials 50} 1 => 1)
  (for-each-failure (note-that parse-error-found, (fact-failed-with-note error-regexp))))

(silent-formula "a doc string" [a 1] a => 1 a => 1)
(note-that parse-error-found, (fact-failed-with-note #"There are too many expections in your formula form"))

(silent-formula {:foo 5 :bar 6 :num-trials 5} [a 1] a => 1)
(note-that parse-error-found, (fact-failed-with-note #"Invalid keys \(:foo, :bar\) in formula's options map. Valid keys are: :num-trials"))

(let [error-regexp #":num-trials must be an integer 1 or greater"]
  (silent-formula {:num-trials 0 } [a 1] a => 1)
  (silent-formula {:num-trials -1} [a 1] a => 1)
  (silent-formula {:num-trials -2} [a 1] a => 1)
  (silent-formula {:num-trials -3} [a 1] a => 1)
  (silent-formula {:num-trials -4} [a 1] a => 1)
  (for-each-failure (note-that parse-error-found, (fact-failed-with-note error-regexp))))

(defn z [x] )
(silent-formula [a 1]
   (background (h 1) => 5)
   (z a) => 10)
(note-that parse-error-found, (fact-failed-with-note #"background cannot be used inside of formula"))

;; ;; Things that should be valid

(defn k [x] (* 2 (h x)))
;; (formula "against-backgrounds at the front of the body are fine" [a 1]
;;   (against-background (h 1) => 5)
;;   (k a) => 10)

;; :num-trials can be any number 1+
(formula {:num-trials 1} [a 1] a => 1)
(formula {:num-trials 2} [a 1] a => 1)
(formula {:num-trials 3} [a 1] a => 1)
(formula {:num-trials 4} [a 1] a => 1)
(formula {:num-trials 10000} [a 1] a => 1)


;; ;; *num-trials* binding validation

(formula
  "binding too small a value - gives nice error msg"
  [n (gen-int #(< % 1))]
  (binding [*num-trials* n] nil)
     => (throws #"must be an integer 1 or greater"))

(formula
  "allows users to dynamically rebind to 1+"
  [n (gen-int #(>= % 1))]
  (binding [*num-trials* n] nil)
     =not=> (throws Exception))



