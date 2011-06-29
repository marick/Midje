;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-recognizing
  (:use [midje.midje-forms.recognizing])
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(fact "namespacey-match accepts symbols from different midje namespaces"
  (let [values (zip/seq-zip '(m midje.semi-sweet/expect))
        m-node (zip/down values)
        expect-node (-> values zip/down zip/right)]
    (expect (namespacey-match '(m) m-node) => truthy)
    (expect (namespacey-match '(expect) expect-node) => truthy)
    (expect (namespacey-match '(n) m-node) => falsey)))

(tabular 
 (fact "an embedded expect form can be recognized"
   (loc-is-at-full-expect-form? (zip/seq-zip ?form)) => ?expected)

 ?form                                  ?expected
 '(expect x => y)                       truthy
 '(midje.semi-sweet/expect x => y)      truthy
 '(+ x y)                               falsey
 'expect                                falsey)

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) =not=> is-head-of-form-providing-prerequisites?))

(fact "can identify and skip over semi-sweet keywords (currently 'expect' and 'fake')"
  (doseq [skippable '(expect fake midje.semi-sweet/expect midje.semi-sweet/fake)]
    (let [z (zip/seq-zip `(111 (~skippable 1 2 '(3)) "next"))
          skippable (-> z zip/down zip/next zip/down)]
      skippable => is-semi-sweet-keyword?)))

(fact "can ask if at first element of X =?> Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
              "a string" =not=> is-start-of-check-sequence?
              '(foo) =not=> is-start-of-check-sequence?
    
              '( (f 1) ) =not=> is-start-of-check-sequence?
    (possible '( (f 1) )) =not=> is-start-of-check-sequence?
    
              '( (f 1) (f 2)) =not=> is-start-of-check-sequence?
    (possible '( (f 1) (f 2))) =not=> is-start-of-check-sequence?

              '( (f 1) => 2) => is-start-of-check-sequence?
    (possible '( (f 1) => 2)) => is-start-of-check-sequence?

              '( (f 1) =not=> 2) => is-start-of-check-sequence?
    (possible '( (f 1) =not=> 2)) => is-start-of-check-sequence?

              '( (f 1) => 2 :key 'value) => is-start-of-check-sequence?
    (possible '( (f 1) => 2 :key 'value)) => is-start-of-check-sequence?

              '( (f 1) midje.semi-sweet/=> 2) => is-start-of-check-sequence?
    (possible '( (f 1) midje.semi-sweet/=> 2)) => is-start-of-check-sequence?))

(facts "recognizing setup/teardown forms"
  '[ (before :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks) ... ] =not=>  seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after (- 2 2)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after ) ... ] =not=> seq-headed-by-setup-teardown-form?

  '[ (after :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (around :checks (let [x 1] ?form)) ... ] => seq-headed-by-setup-teardown-form?)

(facts "dissecting setup/teardown forms"
  (setup-teardown-bindings '(before :checks (+ 1 1))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1), ?after nil})

  (setup-teardown-bindings '(before :checks (+ 1 1) :after (- 2 2))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1),
                ?after :after, ?second-form (- 2 2)})

  (setup-teardown-bindings '(after :checks (+ 1 1))) =>
    (contains '{?key after, ?when :checks, ?first-form (+ 1 1)})

  (setup-teardown-bindings '(around :checks (let [x 1] ?form))) =>
    (contains '{?key around, ?when :checks,
                ?first-form (let [x 1] ?form) }))

;; Folded prerequisites

(defmacro some-macro [& rest] )


(tabular 
 (fact "things that are not fake-sexps don't need to be unfolded"
   ?thing ?arrow fake-that-needs-unfolding?)

  ;; things not a proper fake macro
  ?thing                                        ?arrow
  '1                                            =not=> 
  '()                                           =not=> 
  '(fake (f (h 1)))                             =not=> ; not in right namespace
  '(midje.semi-sweet/non-fake (f (h 1)))        =not=>

  ;; Sad but true: a cons is not a list.
  (cons 'midje.semi-sweet/fake '((f (h 3)) =test=> 3))    => )

(tabular
 (fact "unfolding depends on the inner structure of a funcall"
  '(midje.semi-sweet/fake ?call =test=> 3) ?arrow fake-that-needs-unfolding?)
   
 ?call                  ?arrow
 ;; Things that might be misinterpreted as nested funcalls
  (f)                  =not=> 
  (f 1)                =not=> 
  (f 1 '(foo))         =not=> 
  (f 1 [foo])          =not=> 
  (f 1 {foo 1})        =not=> 

  ;; These are real nested function calls
  (f (h 1))              => 
  (f 1 (h 1))            => 

  ;; but don't decide to unfold a checker used as argument matcher"
  (f 1 (exactly even?))  =not=>

  ;; don't unfold a constructor.
  (f (java.util.Date. 1 1 1))           =not=>
  (f (new java.util.Date 1 2 2))        =not=>
  
  "Macros are surprisingly hard to get right"
;  '(f 1 (some-macro 33))  =not=> fake-that-needs-unfolding?
  )
