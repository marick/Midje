(ns behaviors.t-for-all
  (:require [midje.data.compendium :as compendium]
            [midje.emission.state :as state]
            [midje.repl :as repl]
            [midje.sweet :refer :all]
            [midje.experimental :refer [for-all gen-let]]
            [midje.test-util :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [midje.parsing.0-to-fact-form.generative :as parser]))

(silent-for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:num-tests 1000}
  (fact (+ strictly-pos any-integer) => pos?))
(note-that fact-fails (failure-was-at-line 17))

(silent-for-all
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:num-tests 1000}
  (fact 1 => 1)
  (+ strictly-pos any-integer) => pos?)
(note-that fact-fails (failure-was-at-line 25))

(silent-gen-let
  [strictly-pos gen/s-pos-int
   smaller (gen/choose (- strictly-pos) strictly-pos)]
  {:num-tests 1000}
  (fact 1 => 1)
  (fact smaller => #(< (- strictly-pos) % strictly-pos)))
(note-that fact-fails (failure-was-at-line 30))
(note-that (fails 1 time))

(silent-for-all "generative tests"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:num-tests 1000}
  (fact "Summing an integer to a positive integer should be positive? Really?"
    strictly-pos => integer?
    (+ strictly-pos any-integer) => pos?))
(note-that (fails 1 time))

(def pass-count (state/output-counters:midje-passes))
(for-all
  [strictly-pos gen/s-pos-int]
  (+ strictly-pos 0) => pos?
  (let [an-int 0]
    (fact "I. you can wrap your facts in `let`"
      (+ an-int strictly-pos) => pos?))
  (let [an-int 1]
    (fact "II. you can wrap your facts in `let`"
      (+ an-int strictly-pos) => pos?)))

(fact
  (state/output-counters:midje-passes) => (+ pass-count 3))

(for-all "random map not confounded with quick-check options if in correct place"
  [strictly-pos gen/s-pos-int]
  {:seed 3510160943861}
  {:some 'random :map '.}
  (fact (+ strictly-pos 0) => pos?))

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

(def pass-count (state/output-counters:midje-passes))
(try
  (silent-for-all
    [x gen/int]
    (fact "Exceptions that occur in fact set up should propagate up and not cause a passing test."
      (/ 1 0)
      x => integer?))
  (catch java.lang.ArithmeticException e))
(fact (state/output-counters:midje-passes) => pass-count)

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

(gen-let [s (gen/return "s")]
  s => "s")

(fact "you can put gen-let inside of a fact"
  (gen-let [s (gen/return "s")]
    s => "s"))

(fact "gen-let composes generators"
  (gen-let [i (gen/elements [1 2 3])
            s (gen/return (str i))]
    (Integer/parseInt s) => i))

(fact "gen-let composes multiple generators"
  (gen-let [i  gen/s-pos-int
            is (gen/vector gen/int i)
            e  (gen/elements is)]
    (count is) => i
    (some #{e} is) => e))

(fact ":let syntax allows for simple functions over generated elements"
  (gen-let [i gen/pos-int
            :let [s  (str i)
                  s2 (str s s)]]
    (Integer/parseInt s2) => (+ (* 10 i) i)))

(fact "a :let block can be in the middle of gen-let bindings"
  (gen-let [i  (gen/choose 0 9)
            :let [s  (str i)
                  s2 (str s s)]
            xs (gen/elements [s s2])]
    (->> xs seq (map #(Character/getNumericValue %)) (some #{i})) => i))

(fact "a :let block can be in the beginning of gen-let bindings"
  (gen-let [:let [i 2
                  j (+ i 2)]
            x (gen/vector gen/int i j)]
    (some #{2 3 4} (count x)) => some?))

(silent-gen-let "generative tests"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:num-tests 1000}
  (fact "Summing an integer to a positive integer should be positive? Really?"
    strictly-pos => integer?
    (+ strictly-pos any-integer) => pos?))
(note-that (fails 1 time))

(silent-gen-let "confounding random map with quick-check options"
  [strictly-pos gen/s-pos-int
   any-integer  gen/int]
  {:some 'random :map '.}
  (fact (+ strictly-pos any-integer) => pos?))
(note-that
  parse-error-found
  (fact-failed-with-note #"unrecognized keys in \`gen-let\` options map: \(:some :map\)"))

(silent-gen-let "uneven count in binding vector"
  [strictly-pos gen/s-pos-int
   any-integer]
  (fact 1 => 1))
(note-that
  parse-error-found
  (fact-failed-with-note #"\`gen-let\` must have an even number"))

(for-all "a `for-all` with no checks inside still works"
  [strictly-pos gen/s-pos-int]
  false)

(silent-gen-let "a `gen-let` with generation boundings still works"
  []
  (fact 1 => 1))
(note-that
  parse-error-found
  (fact-failed-with-note #"\`gen-let\` cannot have an empty binding vector"))

(silent-gen-let
  [y gen/int]
  {:num-tests -1}
  y => integer?)
(note-that
  parse-error-found
  (fact-failed-with-note #":num-tests \`gen-let\` option must be greater than 0: -1"))

(silent-gen-let
  [y gen/int]
  {:num-tests 0}
  y => integer?)
(note-that
  parse-error-found
  (fact-failed-with-note #":num-tests \`gen-let\` option must be greater than 0: 0"))

(defn-call-countable my-gen-let-inc [x] (inc x))

(silent-gen-let
  [x gen/int]
  1 => 2
  (my-gen-let-inc x) => integer?)
(fact "when any fact fails, generated input is shrunk causing all facts to be
      run again"
  @my-gen-let-inc-count => 2)

(def pass-count (state/output-counters:midje-passes))
(try
  (silent-gen-let
    [x gen/int]
    (fact "Exceptions that occur in fact set up should propagate up and not cause a passing test."
      (/ 1 0)
      x => integer?))
  (catch java.lang.ArithmeticException e))
(fact (state/output-counters:midje-passes) => pass-count)



(facts "verifying gen-let macro code generation"
  (fact
    (parser/roll-up-bindings
      `([s (gen/return "s")])
      `(gen/return
         {:args     (list s)
          :function fact-fn-sym
          :result   (fact-fn-sym s)}))
    => `(gen/bind
          (gen/return "s")
          (fn
            [s]
            (gen/return
              {:args     (list s)
               :function fact-fn-sym
               :result   (fact-fn-sym s)}))))

  (fact
    (parser/roll-up-bindings
      `([s (gen/return (str i))]
         [i (gen/elements [1 2 3])])
      `(gen/return
         {:args     (list i s)
          :function fact-fn-sym
          :result   (fact-fn-sym i s)}))
    => `(gen/bind
          (gen/elements [1 2 3])
          (fn [i]
            (gen/bind
              (gen/return (str i))
              (fn
                [s]
                (gen/return
                  {:args     (list i s)
                   :function fact-fn-sym
                   :result   (fact-fn-sym i s)}))))))

  (parser/roll-up-bindings
    `([:let [s (str i)]]
       [i (gen/elements [1 2 3])])
    `(gen/return
       {:args     (list i s)
        :function fact-fn-sym
        :result   (fact-fn-sym i s)})))

