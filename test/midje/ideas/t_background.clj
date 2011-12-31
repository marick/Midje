;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-background
  (:require [clojure.zip :as zip])
  (:use [midje sweet test-util]
        [midje.internal-ideas.wrapping :only [for-wrapping-target?]]
        [midje.util unify]
        [midje.error-handling monadic]
        [midje.ideas.background :only [separate-background-forms setup-teardown-bindings
                                 seq-headed-by-setup-teardown-form? background-wrappers]]))
(testable-privates midje.ideas.background
                   extract-state-descriptions+fakes state-wrapper)

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])



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
)


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

(facts "recognizing setup/teardown forms"
  '[ (before :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks) ... ] =not=>  seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after (- 2 2)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (before :checks (+ 1 1) :after ) ... ] =not=> seq-headed-by-setup-teardown-form?

  '[ (after :checks (+ 1 1)) ... ] => seq-headed-by-setup-teardown-form?
  '[ (around :checks (let [x 1] ?form)) ... ] => seq-headed-by-setup-teardown-form?)




;; wrapping

(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (extract-state-descriptions+fakes []) => []
  (extract-state-descriptions+fakes '[(f 1) => 2]) 
  => '[(midje.semi-sweet/fake (f 1) => 2 :background :background
                                         :times (range 0))]
  (extract-state-descriptions+fakes '[   (f 1) => 2 :foo 'bar (f 2) => 33 ])
  => '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar
                                         :background :background
                                         :times (range 0))
       (midje.semi-sweet/fake (f 2) => 33 :background :background
                              :times (range 0)) ]

  "data fakes"
  (extract-state-descriptions+fakes '[...m... =contains=> {:a 1, :b 2}])
  => '[(midje.semi-sweet/data-fake ...m... =contains=> {:a 1, :b 2})]

  "other types are left alone"
  (extract-state-descriptions+fakes
   '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checks (swap! test-atom (constantly 0))) ]

 "mixtures"
 (extract-state-descriptions+fakes
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
(defn- form-matching? [expected]
  (fn [actual] (= actual
                  (substitute expected {'?danger 'midje.ideas.t-background/?form}))))

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
    final => (for-wrapping-target? :checks))
)

(facts "about safe expansion of weird forms"
  (map? {1 'do}) => truthy
  (first (second '(midje.semi-sweet.expect (midje.sweet.fact 1 => 2)))) => 'midje.sweet.fact
  (set? #{1 'do}) => truthy)

 ;; Validation unit facts

(tabular
  (facts "before, after and around validation"
    (fact "valid, then return rest of form"
      (validate (cons ?wrapper `(:facts (do "something")))) => `(:facts (do "something")))
  
    (fact "wrapper's must use either :facts, :contents, or checks as their wrapping targets"
      (validate (cons ?wrapper `(:abc (do "something")))) => user-error-form?)
    
    (fact "correct form length" 
      (validate (cons ?wrapper `(:facts (do "something") (do "another thing")))) => user-error-form?
      (validate (list ?wrapper)) => user-error-form? ))

    ?wrapper
    'before 
    'after  
    'around)

(fact "before gets an optional :after param"
  (validate `(before :contents (do "something") :after (do "another thing"))) =not=> user-error-form?
  (validate `(before :contents (do "something") :around (do "another thing"))) => user-error-form?)

(fact "after and around don't get extra params - length should be 3"
  (validate `(after :contents (do "something") :after (do "another thing"))) => user-error-form?
  (validate `(around :contents (do "something") :after (do "another thing"))) => user-error-form?)

(facts "against-background validation"

  (fact "valid, then return rest of form"
    (validate `(against-background [(before :contents (do "something")) 
                                    (after :checks (do "something"))]
                 "body")) => `([(before :contents (do "something")) 
                                          (after :checks (do "something"))] "body")
  
    (validate `(against-background (before :contents (do "something")) 
                 "body")) 
    => 
    `( (before :contents (do "something")) 
         "body") )
    
  (fact "invalid if any state-description invalid"
    (validate `(against-background [(before :contents (do "something"))
                                    (after :BAD (do "something"))]
                 "body")) => user-error-form?
    (validate `(against-background (before :BAD (do "something"))
                 "body")) => user-error-form? ) 
  
  (fact "invalid when the second in form is not state-descriptions and/or bckground fakes" 
    (validate `(against-background :incorrect-type-here "body")) =future=> user-error-form? )
  
  (fact "invalid when form has less than 3 elements" 
    (validate `(against-background [(before :contents (do "something"))
                                    (after :BAD (do "something"))])) => user-error-form? 
    (validate `(against-background (before :contents (do "something")))) => user-error-form? ))

(facts "background validation"

  (fact "valid, then return rest of form"
    (validate `(background (before :contents (do "something")) 
                           (after :checks (do "something")))) 
    
    => `( (before :contents (do "something")) 
          (after :checks (do "something")))
  
    (validate `(background (before :contents (do "something")))) 
    => 
    `( (before :contents (do "something"))))
    
  (fact "invalid if any state-description invalid"
    (validate `(background (before :contents (do "something"))
                           (after :BAD (do "something")))) => user-error-form?
    (validate `(background (before :BAD (do "something")))) => user-error-form? ) )  

 ;; Validation end-to-end facts


;;;;;;;;;;;;;;;;;;;;;;;; ** `against-background` end-to-end ** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ~~ Vectory

;; check invalid wrapping targets
(after-silently
  (against-background [(before :invalid-wrapping-target (do "something"))] 
    "body")

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

;; check for vectors w/ no state-descriptions or background fakes
(after-silently
  (against-background [:not-a-state-description-or-fake]
    (fact nil => nil))

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

(defn f [] )

;; check for vectors w/ one thing that isn't a state-description or background fake
(after-silently
  (against-background [(before :contents (do "something")) (f) => 5 :other-odd-stuff]
    (fact nil => nil))

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

;; check for 
(after-silently
  (against-background []
    (fact nil => nil))

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

;; ~~Sequency 

;; check invalid wrapping targets
(after-silently
  (against-background (before :invalid-wrapping-target (do "something")) 
    "body")

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

; check for vectors w/ one thing that isn't a state-description or background fake
(after-silently
  (against-background :invalid-stuff-here
    (fact nil => nil))

  (fact 
    @reported => (one-of (contains {:type :user-error}))))

;; check for 
(after-silently
  (against-background
    (fact nil => nil))

  (fact 
    @reported => (one-of (contains {:type :user-error}))))
           

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ** `background` end-to-end ** ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check invalid wrapping targets
(after-silently
  (background (before :invalid-wrapping-target (do "something")))
  
  (fact 
    @reported => (one-of (contains {:type :user-error}))))


