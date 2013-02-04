(ns midje.parsing.1-to-explicit-form.t-background
  (:require [clojure.zip :as zip])
  (:use [midje sweet test-util]
        [midje.parsing.util.wrapping :only [for-wrapping-target?]]
        [midje.util unify]
        [midje.error-handling validation-errors]
        [midje.parsing.1-to-explicit-form.background :only [separate-background-forms 
                                 background-wrappers]]
        midje.util))
(expose-testables midje.parsing.1-to-explicit-form.background)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])

(prn "details/t_background should have tests for line numbers")

(tabular
 (fact "separate-background-forms divides forms into background and other things"
   (separate-background-forms '?in) => '[ ?background-forms  ?other-forms])

 ?in                                    ?background-forms               ?other-forms
 [ (against-background ..back1..
                       ..back2..)
   ..claim1..
   ..claim2..]                         [..back1.. ..back2..]          [..claim1.. ..claim2..]

 [ (against-background ..back1..)
   ..claim1..
   (against-background ..back2..)]      [..back1.. ..back2..]         [..claim1..] 

 []                                     []                              []
 [ (f 1) => 3 ]                         []                              [ (f 1) => 3 ]

 [(against-background)
  (f 1) => 3 ]                          []                              [(f 1) => 3]

 ;; It's OK to use background instead of against-background
 [ (background ..back1..
               ..back2..)
   ..claim1..
   ..claim2..]                         [..back1.. ..back2..]          [..claim1.. ..claim2..]

 ;; It's OK for against-background in a fact to have a vector of background changers
 [ (background [..back1.. ..back2..])
   ..claim1..
   (against-background ..back3..)
   ..claim2..]                         [..back1.. ..back2.. ..back3..] [..claim1.. ..claim2..]

 ;; An embedded wrapping against-background is not identified as an against-background.
   [ (against-background [..back1.. ..back2..]
       (fact "an embedded fact"))]
                                       []                               [ (against-background [..back1.. ..back2..]
                                                                            (fact "an embedded fact"))]
     

)



;; wrapping

(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (extract-background-changers []) => []
  (extract-background-changers '[(f 1) => 2]) 
  => '[(midje.semi-sweet/fake (f 1) => 2 :background :background
                                         :times (range 0))]
  (extract-background-changers '[   (f 1) => 2 :foo 'bar (f 2) => 33 ])
  => '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar
                                         :background :background
                                         :times (range 0))
       (midje.semi-sweet/fake (f 2) => 33 :background :background
                              :times (range 0)) ]

  "data fakes"
  (extract-background-changers '[...m... =contains=> {:a 1, :b 2}])
  => '[(midje.semi-sweet/data-fake ...m... =contains=> {:a 1, :b 2})]

  "other types are left alone"
  (extract-background-changers
   '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checks (swap! test-atom (constantly 0))) ]

 "mixtures"
 (extract-background-changers
  '[ (f 1) => 2 (before :checks (swap! test-atom (constantly 0))) (f 2) => 3 ])
 => '[ (midje.semi-sweet/fake (f 1) => 2 :background :background
                                        :times (range 0))
      (before :checks (swap! test-atom (constantly 0)))
      (midje.semi-sweet/fake (f 2) => 3 :background :background
                                        :times (range 0)) ]
 )

(defn guard-special-form [bindings]
  (assoc (dissoc bindings '?danger) '?danger (str (bindings '?danger))))

(defmacro wrapping-form-is [ original expected ]
  (let [bindings (unify expected (state-wrapper original)) ]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" }))

;; The magical symbol that's used in wrapper substitution can't be used in
;; a fact because it gets substituted. So we let the caller use "danger" instead.
(letfn [(form-matching? [expected]
          (fn [actual] (= actual
                         (substitute expected {'?danger 'midje.parsing.1-to-explicit-form.t-background/?form}))))]

  (fact "canonicalized setup/teardown wrappers can be put into final form"
    (let [final (state-wrapper '(before :checks (do-something)))]
      final => (form-matching? '(try (do-something) ?danger (finally nil)))
      final => (for-wrapping-target? :checks))
  
    (let [final (state-wrapper '(before :facts (do-something) :after (finish)))]
      final => (form-matching? '(try (do-something) ?danger (finally (finish))))
      final => (for-wrapping-target? :facts))
  
    (let [final (state-wrapper '(after :all (do-something)))]
      final => (form-matching? '(try ?danger (finally (do-something))))
      final => (for-wrapping-target? :all))
  
    (let [final (state-wrapper '(around :checks (let [x 1] ?form)))]
      final => (form-matching? '(let [x 1] ?danger))
      final => (for-wrapping-target? :checks)))
)

(facts "about safe expansion of weird forms"
  (map? {1 'do}) => truthy
  (first (second '(midje.semi-sweet.expect (midje.sweet.fact 1 => 2)))) => 'midje.sweet.fact
  (set? #{1 'do}) => truthy)

(let [around-facts-call-count (atom 0)]
  (against-background [(around :facts (do (swap! around-facts-call-count inc) ?form))]
    (fact "around-facts is only called once per fact"
      @around-facts-call-count => 1)))

