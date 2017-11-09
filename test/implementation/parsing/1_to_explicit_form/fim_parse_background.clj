(ns implementation.parsing.1-to-explicit-form.fim-parse-background
  (:require [clojure.zip :as zip]
            [midje.sweet :refer :all]
            [midje.parsing.1-to-explicit-form.parse-background :as parse-background]
            [midje.parsing.2-to-lexical-maps.data-fakes :refer [data-fake]]
            [midje.parsing.2-to-lexical-maps.expects :refer [expect]]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.util.wrapping :refer [for-wrapping-target?]]
            [midje.test-util :refer :all]
            [midje.util :refer :all]
            [midje.util.unify :as unify]
            [pointer.core :as pcore]))
(expose-testables midje.parsing.1-to-explicit-form.parse-background)


(tabular
 (fact "extract-non-wrapping-background-forms pulls out forms to be wrapped around entire container"
   (parse-background/separate-extractable-background-changing-forms '?in) => '[ ?background-forms  ?other-forms])

 ?in                                    ?background-forms               ?other-forms
 [ (against-background ..back1..
                       ..back2..)
   ..body1..
   ..body2..]                           [..back1.. ..back2..]           [..body1.. ..body2..]

 [ (against-background ..back1..)
   ..body1..
   (against-background ..back2..)]      [..back1.. ..back2..]           [..body1..]

 []                                     []                              []
 [ (f 1) => 3 ]                         []                              [ (f 1) => 3 ]

 [(against-background)
  (f 1) => 3 ]                          []                              [(f 1) => 3]

 ;; It's OK to use `background` instead of `against-background`
 [ (background ..back1..
               ..back2..)
   ..body1..
   ..body2..]                         [..back1.. ..back2..]              [..body1.. ..body2..]

 ;; It's OK for background in a fact to have a vector of background changers
 [ (background [..back1.. ..back2..])
   ..body1..
   (against-background ..back3..)
   ..body2..]                         [..back1.. ..back2.. ..back3..]   [..body1.. ..body2..]

 ;; prerequisite forms are also extracted
   [ (prerequisite (f) => 1)
     (prerequisites (g) => 1)
     ..body1..
     (prerequisite [(h) => 1])
     (prerequisites [(j) => 1])
     ..body2..]                       [(f) => 1
                                       (g) => 1
                                       (h) => 1
                                       (j) => 1]              [..body1.. ..body2..]

 ;; wrapping background changers are not extracted
   [ (against-background [..back1.. ..back2..]
       (fact "an embedded fact"))]
                                       []                     [ (against-background [..back1.. ..back2..]
                                                                   (fact "an embedded fact"))]
   [ (with-state-changes [..back1.. ..back2..]
       (fact "an embedded fact"))]
                                       []                     [ (with-state-changes [..back1.. ..back2..]
                                                                   (fact "an embedded fact"))]
)



(fact "Collections of background changers are syntactically ornate, but they can be separated"
  (fact "ordinary prerequisites are converted to fakes"
    (separate-individual-changers []) => []
    (separate-individual-changers `[(f 1) => 2])
    => `[(fake (f 1) => 2 :position (pcore/line-number-known nil)
                          :background :background
                          :times (range 0))]
    (separate-individual-changers `[(f 1) => 2 :foo 'bar (f 2) => 33])
    => `[(fake (f 1) => 2 :foo 'bar
                          :position (pcore/line-number-known nil)
                          :background :background
                          :times (range 0))
         (fake (f 2) => 33 :position (pcore/line-number-known nil)
                           :background :background
                           :times (range 0))])

  (fact "metaconstant `=contains=>` become data fakes"
    (separate-individual-changers `[...m... =contains=> {:a 1, :b 2}])
    => `[(data-fake ...m... =contains=> {:a 1, :b 2}
                    :position (pcore/line-number-known nil)
                    :background :background
                    :times (range 0))])

  (fact "state changers are simply extracted as-is: no expansion"
    (separate-individual-changers
     '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
     '[ (before :checks (swap! test-atom (constantly 0))) ])

  (fact "mixtures"
    (separate-individual-changers `[ (f 1) => 2 (before :checks (swap! test-atom (constantly 0))) (f 2) => 3 ])
    => `[ (fake (f 1) => 2 :position (pcore/line-number-known nil)
                           :background :background
                           :times (range 0))
          (before :checks (swap! test-atom (constantly 0)))
          (fake (f 2) => 3 :position (pcore/line-number-known nil)
                           :background :background
                           :times (range 0)) ]))


;; The magical symbol that's used in wrapper substitution can't be used in
;; a fact because it gets substituted. So we let these facts use `?the-unification-symbol` instead.
(letfn [(form-matching? [expected]
          (chatty-checker [actual] (= actual
                                      (unify/substitute expected {'?the-unification-symbol `?form}))))]

  (fact "state-changers can be turned into unification templates"
    (let [template (make-state-unification-template '(before :checks (do-something)))]
      template => (form-matching? '(try (do-something) ?the-unification-symbol (finally nil)))
      template => (for-wrapping-target? :checks))

    (let [template (make-state-unification-template '(before :facts (do-something) :after (finish)))]
      template => (form-matching? '(try (do-something) ?the-unification-symbol (finally (finish))))
      template => (for-wrapping-target? :facts))

    (let [template (make-state-unification-template '(after :all (do-something)))]
      template => (form-matching? '(try ?the-unification-symbol (finally (do-something))))
      template => (for-wrapping-target? :all))

    (let [template (make-state-unification-template '(around :checks (let [x 1] ?form)))]
      template => (form-matching? '(let [x 1] ?the-unification-symbol))
      template => (for-wrapping-target? :checks)))
)



(facts "about safe expansion of weird forms"
  (map? {1 'do}) => truthy
  (first (second `(expect (midje.sweet/fact 1 => 2)))) => `fact
  (set? #{1 'do}) => truthy)

(let [around-facts-call-count (atom 0)]
  (against-background [(around :facts (do (swap! around-facts-call-count inc) ?form))]
    (fact "around-facts is only called once per fact"
      @around-facts-call-count => 1)))

