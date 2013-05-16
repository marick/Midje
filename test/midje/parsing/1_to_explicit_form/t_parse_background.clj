(ns midje.parsing.1-to-explicit-form.t-parse-background
  (:use midje.sweet
        midje.test-util
        [midje.parsing.util.wrapping :only [for-wrapping-target?]]
        [midje.parsing.2-to-lexical-maps.expects :only [expect]]
        [midje.parsing.2-to-lexical-maps.fakes :only [fake]]
        [midje.parsing.2-to-lexical-maps.data-fakes :only [data-fake]]
        midje.util)
  (:require [clojure.zip :as zip]
            [midje.util.unify :as unify]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.1-to-explicit-form.parse-background :as parse-background]))
(expose-testables midje.parsing.1-to-explicit-form.parse-background)


(tabular
 (fact "separate-background-forms divides forms into background and other things"
   (parse-background/separate-background-forms '?in) => '[ ?background-forms  ?other-forms])

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

 ;; A wrapping against-background is not identified as an against-background.
   [ (against-background [..back1.. ..back2..]
       (fact "an embedded fact"))]
                                       []                               [ (against-background [..back1.. ..back2..]
                                                                            (fact "an embedded fact"))]
)



;; wrapping

(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (extract-background-changers []) => []
  (extract-background-changers `[(f 1) => 2]) 
  => `[(fake (f 1) => 2 :background :background
                        :times (range 0))]
  (extract-background-changers `[   (f 1) => 2 :foo 'bar (f 2) => 33 ])
  => `[(fake (f 1) => 2 :foo 'bar
                        :background :background
                        :times (range 0))
       (fake (f 2) => 33 :background :background
                         :times (range 0)) ]

  "data fakes"
  (extract-background-changers `[...m... =contains=> {:a 1, :b 2}])
  => `[(data-fake ...m... =contains=> {:a 1, :b 2}
                  :background :background
                   :times (range 0))]

  "other types are left alone"
  (extract-background-changers
   '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checks (swap! test-atom (constantly 0))) ]

 "mixtures"
 (extract-background-changers
  `[ (f 1) => 2 (before :checks (swap! test-atom (constantly 0))) (f 2) => 3 ])
 => `[ (fake (f 1) => 2 :background :background
                        :times (range 0))
      (before :checks (swap! test-atom (constantly 0)))
      (fake (f 2) => 3 :background :background
                       :times (range 0)) ]
 )

(defn guard-special-form [bindings]
  (assoc (dissoc bindings '?danger) '?danger (str (bindings '?danger))))

(defmacro wrapping-form-is [ original expected ]
  (let [bindings (unify/unify expected (state-wrapper original)) ]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" }))

;; The magical symbol that's used in wrapper substitution can't be used in
;; a fact because it gets substituted. So we let the caller use "danger" instead.
(letfn [(form-matching? [expected]
          (chatty-checker [actual] (= actual
                                      (unify/substitute expected {'?danger `?form}))))]

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
  (first (second `(expect (midje.sweet/fact 1 => 2)))) => `fact
  (set? #{1 'do}) => truthy)

(let [around-facts-call-count (atom 0)]
  (against-background [(around :facts (do (swap! around-facts-call-count inc) ?form))]
    (fact "around-facts is only called once per fact"
      @around-facts-call-count => 1)))

