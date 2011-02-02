;; -*- indent-tabs-mode: nil -*-

(ns midje.t-semi-sweet
  (:use [clojure.test]  ;; This is used to check production mode with deftest.
        [midje.sweet]
        [midje.util form-utils]
        [midje.test-util]))
(testable-privates midje.semi-sweet fakes-and-overrides)

(unfinished faked-function mocked-function other-function)

(defchecker odd-checker
  [actual]
  (odd? actual))

(fact "separating overrides of an #expect from fakes"
  ;; The lets are because fact isn't smart enough not to add overrides to fake call otherwise.
  (let [actual (fakes-and-overrides '( (fake (f 1) => 2) :key 'value))]
    actual => [  '[(fake (f 1) => 2)]
                 '[:key 'value] ])

  (let [actual (fakes-and-overrides '( (not-called some-function) :key 'value))]
    actual => [ '[(not-called some-function)]
                '[:key 'value] ])

  ;; often passed a seq.
  (let [actual (fakes-and-overrides (seq '( (fake (f 1) => 2) :key 'value)))]
    actual => [  '[(fake (f 1) => 2)]
                 '[:key 'value] ])

  (let [actual (fakes-and-overrides '())]
    actual => (just empty? empty?)))

(fact "calling a faked function raises an error"
  (faked-function) => (throws Error))

(facts "about the creation of fake maps"
  (let [some-variable 5
        previous-line-position (file-position 1)
        fake-0 (fake (faked-function) => 2)
        fake-1 (fake (faked-function some-variable) => (+ 2 some-variable))
        fake-2 (fake (faked-function 1 some-variable) => [1 some-variable])
        fake-streamed (fake (faked-function 0) =streams=> ['r1 'r2])]

    "The basic parts"
    (:function fake-0) => #'midje.t-semi-sweet/faked-function
    (:call-text-for-failures fake-1) => "(faked-function some-variable)"
    (deref (:count-atom fake-0)) => 0

    "argument matching"
    (count (:arg-matchers fake-0)) => 0

    "Note that lexical scoping is obeyed"
    (count (:arg-matchers fake-1)) => 1
    (apply-pairwise (:arg-matchers fake-1) [5] [nil]) => [[true] [false]]
    (count (:arg-matchers fake-2)) => 2
    (apply-pairwise (:arg-matchers fake-2) [5 5] [1 1]) => [  [false true]
                                                              [true false] ]

    "Result supplied"
    ((:result-supplier fake-0)) => 2
    ((:result-supplier fake-1)) => (+ 2 some-variable)
    ((:result-supplier fake-2)) => [1 some-variable]

    "Streamed results"
    ((:result-supplier fake-streamed)) => 'r1
    ((:result-supplier fake-streamed)) => 'r2))

(facts "key-value arguments can override fakes"
  (let [fake (fake (faked-function) => 2 :file-position 33)]
    (fake :file-position) => 33)

  (let [filepos 33
        fake (fake (faked-function) => 2 :file-position filepos)]
    (:file-position fake) => 33))

(facts "about not-called"
  (let [fake-0 (not-called faked-function)]

    (:function fake-0) => #'midje.t-semi-sweet/faked-function
    (:call-text-for-failures fake-0) => "faked-function was called."
    @(:count-atom fake-0) => 0
    (:arg-matchers fake-0) => nil?
    ((:result-supplier fake-0)) => nil?))

(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(facts "about expect"
  "success"
  (after-silently 
   (expect (+ 1 3) => 4)
   @reported => (one-of pass))

  "actual doesn't match expected"
  (after-silently 
   (expect (+ 1 3) => nil)
   @reported => (just (contains {:type :mock-expected-result-failure
                                 :actual 4
                                 :expected nil})))

  "not-called in the first position"
  (after-silently 
   (expect (function-under-test) => 33
           (not-called no-caller)
           (fake (mocked-function) => 33))
   @reported => (one-of pass))

  "not-called in last position"
  (after-silently 
   (expect (function-under-test) => 33
           (fake (mocked-function) => 33)
           (not-called no-caller))
   @reported => (one-of pass))

  "mocked calls go fine, but function under test produces the wrong result"
  (after-silently
   (expect (function-under-test 33) => 12
           (fake (mocked-function 33) => (not 12) ))
   @reported => (just (contains {:actual false
                                 :expected 12})))

  "mock call supposed to be made, but wasn't (zero call count)"
  (after-silently
   (expect (no-caller) => "irrelevant"
           (fake (mocked-function) => 33))
   @reported => (just wrong-call-count bad-result))

  "mock call was not supposed to be made, but was (non-zero call count)"
  (after-silently
   (expect (function-under-test 33) => "irrelevant"
           (not-called mocked-function))
   @reported => (just wrong-call-count bad-result))

  "call not from inside function"
  (after-silently 
   (expect (+ (mocked-function 12) (other-function 12)) => 12
           (fake (mocked-function 12) => 11)
           (fake (other-function 12) => 1))
   @reported => (just pass))

  "call that matches none of the expected arguments"
  (after-silently
   (expect (+ (mocked-function 12) (mocked-function 33)) => "result irrelevant because of earlier failure"
           (fake (mocked-function 12) => "hi"))
   @reported => (just (contains {:type :mock-argument-match-failure :actual '(33)})
                      bad-result))

  "failure because one variant of multiply-mocked function is not called"
  (after-silently 
   (expect (+ (mocked-function 12) (mocked-function 22)) => 3
           (fake (mocked-function 12) => 1)
           (fake (mocked-function 22) => 2)
           (fake (mocked-function 33) => 3))
   @reported => (just (contains {:type :mock-incorrect-call-count
                                 :expected-call "(mocked-function 33)" })
                      pass)) ; Right result, but wrong reason.

  "multiple calls to a mocked function are perfectly fine"
  (expect (+ (mocked-function 12) (mocked-function 12)) => 2
          (fake (mocked-function 12) => 1) )
  )


(facts "about overriding values in an expect"
  (after-silently
   (expect (function-under-test 1) => 33 :expected-result "not 33"
           (fake (mocked-function 1) => "not 33"))
   @reported => (just pass))

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
    

(fact "expect returns an appropriate truth value"
  (run-silently (expect (function-under-test 1) => 33
                        (fake (mocked-function 1) => 33))) => true
                        
  (run-silently (expect (function-under-test 1) => 33
                         (fake (mocked-function 2) => 33))) => false ; mock failure
  (run-silently (expect (+ 1 1) => 33)) => false)

(facts "about checkers"
  "expected results can be functions"
  (expect (+ 1 1) => even?)

  "exact function matches can be checked with exactly"
  (let [myfun (fn [] 33)
        funs [myfun]]
    (expect (first funs) => (exactly myfun)))

  "mocked function argument matching uses function-aware equality"
  (expect (function-under-test 1 "floob" even?) => even?
          (fake (mocked-function odd-checker anything (exactly even?)) => 44)))

(defn actual-plus-one-is-greater-than [expected]
  (chatty-checker [actual] (> (inc actual) expected)))

(fact "expect can also use chatty checkers"
  "chatty failures provide extra information"
  (after-silently
   (expect (+ 1 1) => (actual-plus-one-is-greater-than 33))
   @reported => (just (contains {:type :mock-expected-result-functional-failure
                                 :actual 2
                                 :intermediate-results [ [ '(inc actual) 3 ] ]
                                 :expected '(actual-plus-one-is-greater-than 33)})))

  "chatty checkers can be used anonymously, like functions"
  (after-silently 
   (expect (+ 1 1) => (chatty-checker [actual] (> (inc actual) 33)))
   @reported => (just (contains {:type :mock-expected-result-functional-failure
                                 :actual 2
                                 :intermediate-results [ [ '(inc actual) 3 ] ]
                                 :expected '(chatty-checker [actual] (> (inc actual) 33))}))))

(declare chatty-prerequisite)
(defn chatty-fut [x] (chatty-prerequisite x))

(fact "chatty functions can be used for argument matching"
  (expect (chatty-fut 5) => "hello"
          (fake (chatty-prerequisite (actual-plus-one-is-greater-than 5)) => "hello")))

(fact "you can fake a function from another namespace"
  (let [myfun (fn [x] (list x))]
    (expect (myfun 1) => :list-called
            (fake (list 1) => :list-called))))

(use 'clojure.set)
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


;; The files to be loaded will blow up unless we're in production-mode.
(binding [clojure.test/*load-tests* false]
  (load "semi_sweet_compile_out"))
  
(binding [midje.semi-sweet/*include-midje-checks* false]
  (load "semi_sweet_compile_out"))


(facts "about result suppliers used"
       (fact "returns identity for =>"
            ;TODO: i think the dsl is matching "=>" as plain =>
             (let [arrow "=>"]
               ((make-result-supplier arrow [1 2 3])) => [1 2 3]))
             
       (fact "returns stream for =streams=>"
             (let [supplier (make-result-supplier "=streams=>" [1 2 3])]
               (supplier) => 1
               (supplier) => 2
               (supplier) => 3)))
                    
