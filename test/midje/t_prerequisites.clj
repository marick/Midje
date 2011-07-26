;; -*- indent-tabs-mode: nil -*-

(ns midje.t-prerequisites
  (:use midje.prerequisites
        [midje.expect :only [expect?]]
        [midje.metaconstants :only [metaconstant-for-form]]
        midje.sweet midje.test-util)
  (:require [clojure.zip :as zip]))

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) =not=> is-head-of-form-providing-prerequisites?))

(fact "can convert prerequisites into fake calls"
  (let [original '( provided                        (f 1) => 3                         (f 2) => (+ 1 1))
        translated '(        (midje.semi-sweet/fake (f 1) => 3) (midje.semi-sweet/fake (f 2) => (+ 1 1)))
        z (zip/seq-zip original)
        loc (zip/down z)]
    (expand-prerequisites-into-fake-calls loc) => translated))

(fact "prerequisite containers are deleted so their contents can be inserted elsewhere"
  (let [original '( (expect (f x) => (+ 1 2)) (provided ...) "next")
        edited   '( (expect (f x) => (+ 1 2))                "next")
        z (zip/seq-zip original)
        original-loc (-> z zip/down zip/right zip/down)
        resulting-loc
         (delete_prerequisite_form__then__at-previous-full-expect-form original-loc)]
        
    original-loc => is-head-of-form-providing-prerequisites?
    resulting-loc => expect?
    (zip/root resulting-loc) => edited))
    



;; Folded prerequisites

(defmacro some-macro [& rest] )


(tabular 
 (fact "things that are not fake-sexps don't need to be unfolded"
   ?thing ?arrow folded-prerequisite?)

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
  '(midje.semi-sweet/fake ?call =test=> 3) ?arrow folded-prerequisite?)
   
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
;  '(f 1 (some-macro 33))  =not=> folded-prerequisite?
  )
;; unfolding prerequisites
(facts "about each step of unfolding"
  "unfolding a non-fake just moves the head of the list"
  (unfolding-step '[...] '[blah]   {}) => [ '[... blah]   [] {} ]
  (unfolding-step '[...] '[(blah)] {}) => [ '[... (blah)] [] {} ]

  "unfolding a plain fake does nothing in particular"
  (unfolding-step '[...] '[(midje.semi-sweet/fake (f 1) =test=> 4) ...] {})
  => [ '[... (midje.semi-sweet/fake (f 1) =test=> 4)] '[...] {} ]

  "unfolding a fake that should be unfolded adds a new fake"
  (let [original '(midje.semi-sweet/fake (f (h 1)) =test=> 4 ...overrides...)
        flattened '(midje.semi-sweet/fake (f ...h-1...) =test=> 4 ...overrides...)
        generated '(midje.semi-sweet/fake (h 1) => ...h-1... ...overrides...)]
    (unfolding-step '[...]
                    [original '...]
                    {})
    => [ ['... flattened]
         [generated '...]
         '{(h 1) ...h-1...} ]
    (provided
      (augment-substitutions {} original) => '{(h 1) ...h-1...}
      (flatten-fake original '{(h 1) ...h-1...}) => flattened
      (generate-fakes '{(h 1) ...h-1...} [...overrides...]) => [generated]))
  )

(fact "substitutions are augmented by unique nested args in fake"
  (augment-substitutions {} '(fake (f (h 1)))) => '{ (h 1) ...h-1... }
  (provided
    (metaconstant-for-form '(h 1)) => '...h-1...)
  "Which means that already-existing substitutions are reused"
  (augment-substitutions {'(h 1) ...h-1...} '(fake (f (h 1)))) => '{ (h 1) ...h-1... })

(fact "fakes are flattened by making substitutions"
  (flatten-fake '(fake (f (g 1) 2 (h 3)) =test=> 33 ...overrides...)
                '{ (g 1) ...g-1..., (h 3) ...h-1... })
  => '(fake (f ...g-1... 2 ...h-1...) =test=> 33 ...overrides...))

(fact "generated fakes maintain overrrides"
  (let [g-fake '(midje.semi-sweet/fake (g 1) midje.semi-sweet/=> ...g-1... ...overrides...)
        h-fake '(midje.semi-sweet/fake (h 3) midje.semi-sweet/=> ...h-1... ...overrides...)]
    (set (generate-fakes '{ (g 1) ...g-1..., (h 3) ...h-1... } '(...overrides...)))
    => #{g-fake h-fake}))


