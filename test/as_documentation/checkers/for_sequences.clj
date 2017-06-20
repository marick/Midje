(ns as-documentation.checkers.for-sequences
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

                                ;;; Ordinary equality

(fact "As usual in Clojure, the type doesn't matter"
  '(1 2 3) => [1 2 3]
  [1 '(2) 3] => '(1 [2] 3)
  (map inc (range 0 3)) => [1 2 3])


                                ;;; Just

(fact "uses extended equality"
  [1 2 3] => (just [odd? even? odd?])
  ["a" "aa" "aaa"] (just [#"a+" #"a+" #"a+"])

  (fact "as a side note, you can also use `three-of` instead of the previous checkable"
    ["a" "aa" "aaa"] => (three-of #"a+")))


(fact "when `just` takes no options, you can omit brackets"
  [1 2 3] => (just odd? even? odd?))


(fact "you can specify that order is irrelevant"
  [1 3 2] => (just [1 2 3] :in-any-order)

  (fact "and you can even leave out the brackets (which is kind of icky)"
    [2 1 3] => (just 1 2 3 :in-any-order))

  (fact "if you like, you can use a set instead of :in-any-order"
    [3 1 2] => (just #{1 2 3}))

  (fact "Midje tries not to be fooled by committing to too-exact matches"
    [1 3] => (just [odd? 1] :in-any-order)))


(fact "the use of extended-equality is not recursive"
  [[[1]]] =not=> (just [[[odd?]]]))

(fact "you have to do this"
  [[[1]]] => (just (just (just odd?))))


                                ;;; Contains

(fact "contains requires only a subset to match"
  [1 2 3] => (contains even? odd?))

(fact "contains requires a contiguous match"
  [1 2 3] =not=> (contains odd? odd?))

(fact "... but you can avoid that with :gaps-ok"
  [1 2 3] => (contains [odd? odd?] :gaps-ok))

(fact ":in-any-order or set arguments are also supported"
  [5 1 4 2] => (contains [1 2 5] :gaps-ok :in-any-order)
  [5 1 4 2] => (contains #{1 2 5} :gaps-ok))


                                ;;; has-prefix, has-suffix

(fact "has-prefix is anchored to the left"
    [1 2 3] =not=> (has-prefix [2 3])     ; it's not a prefix
    [1 2 3] =>     (has-prefix [1 2])
    [1 2 3] =not=> (has-prefix [2 1])     ; order matters
    [1 2 3] =>     (has-prefix [2 1] :in-any-order)
    [1 2 3] =>     (has-prefix #{2 1}))

(fact "has-suffix is anchored to the left"
    [1 2 3] =>     (has-suffix [2 3])
    [1 2 3] =not=> (has-suffix [1 2])     ; not a suffix
    [1 2 3] =not=> (has-suffix [3 2])     ; order matters
    [1 2 3] =>     (has-suffix [3 2] :in-any-order)
    [1 2 3] =>     (has-suffix #{3 2}))

                                ;;; has

(fact "has applies clojure quantification functions to all values a sequence"
  [2 3 4] =>     (has some odd?)
  [2 4 6] =not=> (has some odd?)

  [2 3 4] =not=> (has every? even?)
  [2 4 6] =>     (has every? even?)

  (fact "this is not that different than `partial`..."
    [2 3 4] => (partial some odd?)))


                                ;;; n-of and friends

(fact "one checker to match exactly N elements."
  ["a"] => (one-of "a")
  [:k :w] => (two-of keyword?)
  ["a" "aa" "aaa"] => (three-of #"a+")
  ;; ...
  [1 3 5 7 9 11 13 15 17] => (nine-of odd?)
  ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j"] => (ten-of string?)

  ;; to go above ten-of
  (repeat 100 "a") => (n-of "a" 100))

(fact "counts must be exact"
  ["a" "a" "a"] =not=> (one-of "a")
  [:k :w :extra :keywords] =not=> (two-of keyword?))



