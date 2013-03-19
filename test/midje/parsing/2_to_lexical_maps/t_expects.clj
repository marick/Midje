(ns midje.parsing.2-to-lexical-maps.t-expects
  (:use clojure.test  ;; This is used to check production mode with deftest.
        midje.sweet
        midje.parsing.2-to-lexical-maps.expects
        midje.test-util
        midje.util)
  (:require [midje.config :as config]
            [midje.util.pile :as pile]
            [midje.parsing.util.recognizing :as recognize]
            [midje.emission.api :as emit]))
(expose-testables midje.parsing.2-to-lexical-maps.expects)
 
(unfinished faked-function mocked-function other-function)

;;;

(fact "expect calls emission functions"
  (tabular "emit/pass"
    (fact
      ?expect-call => anything
      (provided
        (emit/pass) => anything))
    ?expect-call
    (expect 1 => 1)
    (expect 2 =not=> 1)
    (expect 1 => odd?)))

;;; RE-EXAMINE THE USEFULNESS OF THESE TESTS

(defchecker odd-checker
  [actual]
  (odd? actual))

(fact "separating overrides of an #expect from fakes"
  ;; The lets are because fact isn't smart enough not to add overrides to fake call otherwise.
  (let [actual (separate recognize/fake?  '( (fake (f 1) => 2) :key 'value))]
    actual => [  '[(fake (f 1) => 2)]
                 '[:key 'value] ])

  (let [actual (separate recognize/fake?  '( (not-called some-function) :key 'value))]
    actual => [ '[(not-called some-function)]
                '[:key 'value] ])

  ;; often passed a seq.
  (let [actual (separate recognize/fake?  (seq '( (fake (f 1) => 2) :key 'value)))]
    actual => [  '[(fake (f 1) => 2)]
                 '[:key 'value] ])

  (let [actual (separate recognize/fake?  '())]
    actual => (just empty? empty?))

  "data fakes too"
  (let [actual (separate recognize/fake?  '((data-fake ..m.. =contains=> {:a 1}) :key 'value))]
    actual => [  '[(data-fake ..m.. =contains=> {:a 1})]
                 '[:key 'value] ]))


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
    (expect (+ 1 3) => 4))

  (fact "There is a =not=> arrow."
    (expect (+ 1 3) =not=> 5))

  (silent-fact "actual doesn't match expected"
    (expect (+ 1 3) => nil))
  (note-that fact-fails, (fact-expected nil), (fact-actual 4))

  (fact "not-called in the first position"
    (expect (function-under-test) => 33
            (not-called no-caller)
            (fake (mocked-function) => 33)))

  (fact "not-called in last position"
    (expect (function-under-test) => 33
            (fake (mocked-function) => 33)
            (not-called no-caller)))

  (silent-fact "mocked calls go fine, but function under test produces the wrong result"
     (expect (function-under-test 33) => 12
             (fake (mocked-function 33) => (not 12) )))
  (note-that fact-fails, (fact-actual false), (fact-expected 12))

  (silent-fact "mock call supposed to be made, but wasn't (zero call count)"
     (expect (no-caller) => "irrelevant"
             (fake (mocked-function) => 33)))
  (note-that fact-fails, (the-prerequisite-was-never-called))

  ;;; This causes an java.lang.AbstractMethodError in Clojure 1.4,
  ;;; 1.5. Clojure bug?  In any case, since not-called is deprecated
  ;;; and :times 0 works, I'm not that worried about it. Also, it only
  ;;; happens because the function under test uses apply to call the mocked function.
  ;;; Direct call is no problem.
  (future-fact "mock call was not supposed to be made, but was (non-zero call count)"
      (expect (function-under-test 33) => "irrelevant"
              (not-called mocked-function)))
  ; (note-that fact-fails, (the-prerequisite-was-incorrectly-called 1 :time))

  (fact "call not from inside function"
    (expect (+ (mocked-function 12) (other-function 12)) => 12
            (fake (mocked-function 12) => 11)
            (fake (other-function 12) => 1)))

  (silent-fact "failure because one variant of multiply-mocked function is not called"
     (expect (+ (mocked-function 12) (mocked-function 22)) => 3
             (fake (mocked-function 12) => 1)
             (fake (mocked-function 22) => 2)
             (fake (mocked-function 33) => 3)))
  (note-that fact-fails,
             (prerequisite-was-never-called #"mocked-function 33"))

  (fact "multiple calls to a mocked function are perfectly fine"
    (expect (+ (mocked-function 12) (mocked-function 12)) => 2
            (fake (mocked-function 12) => 1) )))


(facts "about overriding values in an expect"
  (expect (function-under-test 1) => 33 :expected-result "not 33"
          (fake (mocked-function 1) => "not 33"))

  (let [expected "not 33"]
    (expect (function-under-test 1) => 33 :expected-result expected
            (fake (mocked-function 1) => "not 33"))))

(fact "if there are duplicate overrides, the last one takes precedence"
  (let [expected "not 33"]
    (expect (function-under-test 1) => 33 :expected-result "to be overridden"
            :expected-result expected
            (fake (mocked-function 1) => "not 33"))
    (expect (function-under-test 1) => 33 :expected-result "to be overridden"
            :expected-result expected
            (fake (mocked-function 1) => 5 :result-supplier "IGNORED"
                  :result-supplier (fn [] expected)))))


(defn backing-function [s] s)


(fact "mocks can be partial: they fall through to any previously defined function"
  (config/with-augmented-config {:partial-prerequisites true}
    (expect (str (backing-function "returned") " " (backing-function "overridden")) => "returned new value"
            (fake (backing-function "overridden") => "new value"))))

(facts "about checkers"
  (fact "expected results can be functions"
    (expect (+ 1 1) => even?))  

  (fact "exact function matches can be checked with exactly"
    (let [myfun (constantly 33)
          funs [myfun]]
      (expect (first funs) => (exactly myfun))))

  (fact "mocked function argument matching uses function-aware equality"
    (expect (function-under-test 1 "floob" even?) => even?
            (fake (mocked-function odd-checker irrelevant (exactly even?)) => 44))))

(defn actual-plus-one-is-greater-than [expected]
  (chatty-checker [actual] (> (inc actual) expected)))

(facts "expect can also use chatty checkers"
  (silent-fact "chatty failures provide extra information"
    (expect (+ 1 1) => (actual-plus-one-is-greater-than 33)))
  (note-that fact-fails, (fact-actual 2), (fact-expected '(actual-plus-one-is-greater-than 33)))
  ;; For some reason the parser gets fooled into sticking an expect inside the intermediate result.
  ;; Hence the =test=>
  (note-that (fact-gave-intermediate-result (inc actual) =test=> 3))

  (silent-fact "chatty checkers can be used anonymously, like functions"
    (expect (+ 1 1) => (chatty-checker [actual] (> (inc actual) 33))))
  (note-that fact-fails, (fact-actual 2), (fact-expected '(chatty-checker [actual] (> (inc actual) 33))))
  (note-that (fact-gave-intermediate-result (inc actual) =test=> 3)))

;;

(declare chatty-prerequisite)
(defn chatty-fut [x] (chatty-prerequisite x))
(fact "chatty functions can be used for argument matching"
  (expect (chatty-fut 5) => "hello"
          (fake (chatty-prerequisite (actual-plus-one-is-greater-than 5)) => "hello")))

(fact "you can fake a function from another namespace"
  (let [myfun (fn [x] (list x))]
    (expect (myfun 1) => :list-called
            (fake (list 1) => :list-called))))

(defn set-handler [set1 set2]
  (if (empty? (intersection set1 set2))
    set1
    (intersection set1 set2)))

(fact "a more indirect use of a function can still be faked"
  (expect (set-handler 'set 'disjoint-set) => 'set
          (fake (intersection 'set 'disjoint-set) => #{}))
  (expect (set-handler 'set 'overlapping-set) => #{'intersection}
          (fake (intersection 'set 'overlapping-set) => #{'intersection})))


;; This test is rather indirect. The function under test returns a lazy seq
;; embedded within a top-level list. If the whole tree isn't evaluated, the
;; test will fail because the fake is never called. (Because fake results are
;; checked before final results, since that results in nicer output.)

(def testfun)
(defn lazy-seq-not-at-top-level []
  (list (map (fn [n] (testfun n)) [1])))

(fact "entire trees are eagerly evaluated"
  (expect (lazy-seq-not-at-top-level) => '((32))
          (fake (testfun 1) => 32)))


(capturing-fact-output
 (config/with-augmented-config {:visible-future true}
   (expect (cons :fred) =future=> 3))
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
 
