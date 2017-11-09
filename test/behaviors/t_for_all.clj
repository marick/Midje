(ns behaviors.t-for-all
  (:require [midje.test-util :refer :all]
            [midje.sweet :refer :all]
            [clojure.test.check.generators :as gen]))

(silent-for-all "generative tests"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 1510160943861}
  (fact "Summing an integer to a positive integer should be positive? Really?"
    strictly-pos => integer?
    (+ strictly-pos any-integer) => pos?))
(note-that (fails 1 time))

(for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 3510160943861}
  (let [an-int 0]
    (fact "I. you can wrap your facts in `let`"
      (+ an-int strictly-pos any-integer) => pos?))
  (let [an-int 1]
    (fact "II. you can wrap your facts in `let`"
      (+ an-int strictly-pos any-integer) => pos?)))

(for-all "random map not confounded with quick-check options if in correct place"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 3510160943861}
  {:some 'random :map '.}
  (fact (+ strictly-pos any-integer) => pos?))

(silent-for-all "confounding random map with quick-check options"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:some 'random :map '.}
  (fact (+ strictly-pos any-integer) => pos?))
(note-that
  parse-error-found
  (fact-failed-with-note #"unrecognized keys in \`for-all\` options map: \(:some :map\)"))

(silent-for-all "uneven count in binding vector"
  [strictly-pos gen/s-pos-int
   any-integer]
  (fact 1 => 1))
(note-that
  parse-error-found
  (fact-failed-with-note #"\`for-all\` must have an even number"))

(for-all "a `for-all` with no checks inside still works"
  [strictly-pos gen/s-pos-int]
  false)

(silent-for-all "a `for-all` with generation boundings still works"
  []
  (fact 1 => 1))
(note-that
  parse-error-found
  (fact-failed-with-note #"\`for-all\` cannot have an empty binding vector"))

(silent-for-all
  [y gen/int]
  {:num-tests -1}
  y => integer?)
(note-that
  parse-error-found
  (fact-failed-with-note #":num-tests \`for-all\` option must be greater than 0: -1"))

(silent-for-all
  [y gen/int]
  {:num-tests 0}
  y => integer?)
(note-that
  parse-error-found
  (fact-failed-with-note #":num-tests \`for-all\` option must be greater than 0: 0"))


(unfinished f)
(for-all "mixing facts and arrows and provides"
  [strictly-pos gen/s-pos-int]
  (fact "use the generated value in provided"
    (f strictly-pos) => 2
    (provided (f strictly-pos) => 2))
  ;; The body of the `for-all` is wrapped in a fact, so it can contain `facts`
  ;; as well as checks
  (f strictly-pos) => 1
  (provided (f strictly-pos) => 1))

(defn-call-countable my-str [s] (str s))

(for-all
  [y gen/int]
  {:num-tests 42}
  (my-str y) => string?)
(fact @my-str-count => 42)

(defn-call-countable my-inc [x] (inc x))

(silent-for-all
  [x gen/int]
  1 => 2
  (my-inc x) => integer?)
(fact "when any fact fails, generated input is shrunk causing all facts to be
      run again"
  @my-inc-count => 2)
