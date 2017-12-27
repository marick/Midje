(ns midje.parsing.2-to-lexical-maps.t-expects
  (:require [clojure.test :refer :all]  ;; This is used to check production mode with deftest.
            [midje.sweet :refer :all]
            [midje.parsing.2-to-lexical-maps.expects :refer :all]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.parsing.2-to-lexical-maps.data-fakes :refer [data-fake]]
            [midje.test-util :refer :all]
            [clojure.set :refer [intersection]]
            [midje.util :refer :all]
            [such.sequences :as seq]
            [midje.config :as config]
            [midje.util.pile :as pile]
            [midje.parsing.util.recognizing :as recognize]
            [midje.emission.api :as emit]))
(expose-testables midje.parsing.2-to-lexical-maps.expects)


;; I don't know why `expect` (and it alone) has to be fully qualified for these
;; tests to pass, but it does. Because these tests are mostly holdovers from when
;; users could use `expect` directly, I'm not overly concerned.


(unfinished faked-function mocked-function other-function)

;;;

(fact "expect calls emission functions"
  (tabular "emit/pass"
    (fact
      ?expect-call => anything
      (provided
        (emit/pass) => anything))
    ?expect-call
    (midje.parsing.2-to-lexical-maps.expects/expect 1 => 1)
    (midje.parsing.2-to-lexical-maps.expects/expect 2 =not=> 1)
    (midje.parsing.2-to-lexical-maps.expects/expect 1 => odd?)))



;;; RE-EXAMINE THE USEFULNESS OF THESE TESTS

(defchecker odd-checker
  [actual]
  (odd? actual))

(fact "separating overrides of an #expect from fakes"
  ;; The lets are because fact isn't smart enough not to add overrides to fake call otherwise.
  (let [actual (seq/bifurcate recognize/fake?  `( (fake (f 1) => 2) :key 'value))]
    actual => [  `[(fake (f 1) => 2)]
                 `[:key 'value] ])

  ;; often passed a seq.
  (let [actual (seq/bifurcate recognize/fake?  (seq `( (fake (f 1) => 2) :key 'value)))]
    actual => [  `[(fake (f 1) => 2)]
                 `[:key 'value] ])

  (let [actual (seq/bifurcate recognize/fake?  '())]
    actual => (just empty? empty?))

  "data fakes too"
  (let [actual (seq/bifurcate recognize/fake?  `((data-fake ..m.. =contains=> {:a 1}) :key 'value))]
    actual => [  `[(data-fake ..m.. =contains=> {:a 1})]
                 `[:key 'value] ]))


(fact "calling a faked function raises an error"
  (faked-function) => (throws Error))

(facts "about the creation of fake maps"
  (let [some-variable 5
        previous-line-position (file-position 1)
        fake-0 (fake (faked-function) => 2)
        fake-1 (fake (faked-function some-variable) => (+ 2 some-variable))
        fake-2 (fake (faked-function 1 some-variable) => [1 some-variable])
        fake-streamed (fake (faked-function 0) =streams=> ['r1 'r2])]

    (fact "The basic parts"
      (:var fake-0) => #'midje.parsing.2-to-lexical-maps.t-expects/faked-function
      (:call-text-for-failures fake-1) => "(faked-function some-variable)"
      (deref (:call-count-atom fake-0)) => 0)

    (fact "Result supplied"
      ((:result-supplier fake-0)) => 2
      ((:result-supplier fake-1)) => (+ 2 some-variable)
      ((:result-supplier fake-2)) => [1 some-variable])

    (fact "Streamed results"
      ((:result-supplier fake-streamed)) => 'r1
      ((:result-supplier fake-streamed)) => 'r2)))

(facts "key-value arguments can override fakes"
  (let [fake (fake (faked-function) => 2 :position 33)]
    (fake :position) => 33)

  (let [filepos 33
        fake (fake (faked-function) => 2 :position filepos)]
    (:position fake) => 33))

(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(facts "about expect"
  (fact "success"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 3) => 4))

  (fact "There is a =not=> arrow."
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 3) =not=> 5))

  (silent-fact "actual doesn't match expected"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 3) => nil))
  (note-that fact-fails, (fact-expected nil), (fact-actual 4))

  (silent-fact "mocked calls go fine, but function under test produces the wrong result"
     (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 33) => 12
             (fake (mocked-function 33) => (not 12) )))
  (note-that fact-fails, (fact-actual false), (fact-expected 12))

  (silent-fact "mock call supposed to be made, but wasn't (zero call count)"
     (midje.parsing.2-to-lexical-maps.expects/expect (no-caller) => "irrelevant"
             (fake (mocked-function) => 33)))
  (note-that fact-fails, (the-prerequisite-was-never-called))

  (fact "call not from inside function"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ (mocked-function 12) (other-function 12)) => 12
            (fake (mocked-function 12) => 11)
            (fake (other-function 12) => 1)))

  (silent-fact "failure because one variant of multiply-mocked function is not called"
     (midje.parsing.2-to-lexical-maps.expects/expect (+ (mocked-function 12) (mocked-function 22)) => 3
             (fake (mocked-function 12) => 1)
             (fake (mocked-function 22) => 2)
             (fake (mocked-function 33) => 3)))
  (note-that fact-fails,
             (prerequisite-was-never-called #"mocked-function 33"))

  (fact "multiple calls to a mocked function are perfectly fine"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ (mocked-function 12) (mocked-function 12)) => 2
            (fake (mocked-function 12) => 1) )))


(facts "about overriding values in an expect"
  (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 1) => 33 :expected-result "not 33"
          (fake (mocked-function 1) => "not 33"))

  (let [expected "not 33"]
    (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 1) => 33 :expected-result expected
            (fake (mocked-function 1) => "not 33"))))

(fact "if there are duplicate overrides, the last one takes precedence"
  (let [expected "not 33"]
    (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 1) => 33 :expected-result "to be overridden"
            :expected-result expected
            (fake (mocked-function 1) => "not 33"))
    (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 1) => 33 :expected-result "to be overridden"
            :expected-result expected
            (fake (mocked-function 1) => 5 :result-supplier "IGNORED"
                  :result-supplier (fn [] expected)))))


(defn backing-function [s] s)


(fact "mocks can be partial: they fall through to any previously defined function"
  (config/with-augmented-config {:partial-prerequisites true}
    (midje.parsing.2-to-lexical-maps.expects/expect (str (backing-function "returned") " " (backing-function "overridden")) => "returned new value"
            (fake (backing-function "overridden") => "new value"))))

(facts "about checkers"
  (fact "expected results can be functions"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 1) => even?))

  (fact "exact function matches can be checked with exactly"
    (let [myfun (constantly 33)
          funs [myfun]]
      (midje.parsing.2-to-lexical-maps.expects/expect (first funs) => (exactly myfun))))

  (fact "mocked function argument matching uses function-aware equality"
    (midje.parsing.2-to-lexical-maps.expects/expect (function-under-test 1 "floob" even?) => even?
            (fake (mocked-function odd-checker irrelevant (exactly even?)) => 44))))

(defn actual-plus-one-is-greater-than [expected]
  (chatty-checker [actual] (> (inc actual) expected)))

(facts "expect can also use chatty checkers"
  (silent-fact "chatty failures provide extra information"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 1) => (actual-plus-one-is-greater-than 33)))
  (note-that fact-fails, (fact-actual 2), (fact-expected '(actual-plus-one-is-greater-than 33)))
  ;; For some reason the parser gets fooled into sticking an expect inside the intermediate result.
  ;; Hence the =test=>
  (note-that (fact-gave-intermediate-result (inc actual) =test=> 3))

  (silent-fact "chatty checkers can be used anonymously, like functions"
    (midje.parsing.2-to-lexical-maps.expects/expect (+ 1 1) => (chatty-checker [actual] (> (inc actual) 33))))
  (note-that fact-fails, (fact-actual 2), (fact-expected '(chatty-checker [actual] (> (inc actual) 33))))
  (note-that (fact-gave-intermediate-result (inc actual) =test=> 3)))

;;

(declare chatty-prerequisite)
(defn chatty-fut [x] (chatty-prerequisite x))
(fact "chatty functions can be used for argument matching"
  (midje.parsing.2-to-lexical-maps.expects/expect (chatty-fut 5) => "hello"
          (fake (chatty-prerequisite (actual-plus-one-is-greater-than 5)) => "hello")))

(fact "you can fake a function from another namespace"
  (let [myfun (fn [x] (list x))]
    (midje.parsing.2-to-lexical-maps.expects/expect (myfun 1) => :list-called
            (fake (list 1) => :list-called))))

(defn set-handler [set1 set2]
  (if (empty? (intersection set1 set2))
    set1
    (intersection set1 set2)))

(fact "a more indirect use of a function can still be faked"
  (midje.parsing.2-to-lexical-maps.expects/expect (set-handler 'set 'disjoint-set) => 'set
          (fake (intersection 'set 'disjoint-set) => #{}))
  (midje.parsing.2-to-lexical-maps.expects/expect (set-handler 'set 'overlapping-set) => #{'intersection}
          (fake (intersection 'set 'overlapping-set) => #{'intersection})))


;; This test is rather indirect. The function under test returns a lazy seq
;; embedded within a top-level list. If the whole tree isn't evaluated, the
;; test will fail because the fake is never called. (Because fake results are
;; checked before final results, since that results in nicer output.)

(def testfun)
(defn lazy-seq-not-at-top-level []
  (list (map (fn [n] (testfun n)) [1])))

(fact "entire trees are eagerly evaluated"
  (midje.parsing.2-to-lexical-maps.expects/expect (lazy-seq-not-at-top-level) => '((32))
          (fake (testfun 1) => 32)))


(capturing-fact-output
 (config/with-augmented-config {:visible-future true}
   (midje.parsing.2-to-lexical-maps.expects/expect (cons :fred) =future=> 3))
 (fact @fact-output => #"WORK TO DO.*on.*cons :fred"))


(defmacro some-macro [arg] `(+ 100 200 ~arg))

(facts "about =expands-to=>"
  :check-only-at-load-time
  (fact "calls macro to expand and compares to (unquoted) list"
    (some-macro 8) =expands-to=> (clojure.core/+ 100 200 8))

  (fact "fails if expansion does not match expected list"
    (silent-fact (some-macro 1) =expands-to=> (clojure.core/- 100 200 1))
    (note-that fact-failed,
               (fact-actual `(clojure.core/+ 100 200 1))
               (fact-expected (quote (clojure.core/- 100 200 1))))))

