(ns behaviors.t-for-all
  (:require [midje.data.compendium :as compendium]
            [midje.emission.state :as state]
            [midje.repl :as repl]
            [midje.sweet :refer :all]
            [midje.experimental :refer [for-all]]
            [midje.test-util :refer :all]
            [clojure.test.check.generators :as gen]))

(silent-for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 1510160943861}
  (fact (+ strictly-pos any-integer) => pos?))
(note-that fact-fails (failure-was-at-line 14))

(silent-for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 1510160943861}
  (fact 1 => 1)
  (+ strictly-pos any-integer) => pos?)
(note-that fact-fails (failure-was-at-line 22))

(silent-for-all "generative tests"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 1510160943861}
  (fact "Summing an integer to a positive integer should be positive? Really?"
    strictly-pos => integer?
    (+ strictly-pos any-integer) => pos?))
(note-that (fails 1 time))

(def pass-count (state/output-counters:midje-passes))
(for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:seed 3510160943861}
  (+ strictly-pos any-integer) => pos?
  (let [an-int 0]
    (fact "I. you can wrap your facts in `let`"
      (+ an-int strictly-pos any-integer) => pos?))
  (let [an-int 1]
    (fact "II. you can wrap your facts in `let`"
      (+ an-int strictly-pos any-integer) => pos?)))

(fact
  (state/output-counters:midje-passes) => (+ pass-count 3))

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

(def gen-count (atom 0))
(def gen-int-with-count
  (gen/sized (fn [size]
               (swap! gen-count inc)
               (gen/choose (- size) size))))

(for-all "an empty `for-all` body is fine; quick-check code is still run"
  [x gen-int-with-count]
  {:num-tests 6})
(fact @gen-count => 6)

(for-all :for-all-test :slow {:priority 5}
  [x gen/int]
  (fact :a-for-all-fact x => integer?))

(fact "`for-all` expressions can be tagged and fetched"
  (let [marked-for-all (repl/fetch-facts *ns* :for-all-test)]
    (count marked-for-all) => 1
    (:for-all-test (meta (first marked-for-all))) => true
    (:priority (meta (first marked-for-all))) => 5))

(fact "tagged facts inside of `for-all` don't get registered"
  (let [marked-facts (repl/fetch-facts *ns* :a-for-all-fact)]
    (count marked-facts) => 0))

(def setup-state (atom nil))
(for-all "with-state-changes works with `for-all`"
  [strictly-pos gen/s-pos-int]
  (with-state-changes [(before :facts (reset! setup-state strictly-pos))
                       (after :facts (reset! setup-state 0))]
    (fact @setup-state => strictly-pos)))
(fact @setup-state => 0)

(fact "you can put for-all inside of a fact"
  (for-all
    [x gen/int]
    x => integer?))
