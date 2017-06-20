(ns midje.parsing.2-to-lexical-maps.t-folded-fakes
  (:use midje.parsing.2-to-lexical-maps.folded-fakes)
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.util :refer :all :as util]))

(util/expose-testables midje.parsing.2-to-lexical-maps.folded-fakes)

;; Folded fakes

(fact "metaconstants can be created to stand in for an expression"
  (with-fresh-generated-metaconstant-names
    (metaconstant-for-form '(g)) => '...g-value-1...
    (metaconstant-for-form '(g)) => '...g-value-2...
    (metaconstant-for-form '(h)) => '...h-value-1...

    "Not fooled by namespaces"
    (metaconstant-for-form '(metaconstant-for-form))
    => '...metaconstant-for-form-value-1...))

(defmacro some-macro [& rest] )

(tabular "things that are not fake-sexps don't need to be unfolded"
(fact ?thing ?arrow folded-fake?)

  ;; things not a proper fake macro
  ?thing                                        ?arrow
  '1                                            =not=>
  '()                                           =not=>
  `(fake (f (h 1)))                             =not=> ; not in right namespace
  '(midje.parsing.2-to-lexical-maps/non-fake (f (h 1)))             =not=>

  ;; Sad but true: a cons is not a list.
  (cons `fake '((f (h 3)) =test=> 3))    => )

(tabular
  (fact "unfolding depends on the inner structure of a funcall"
    (list `fake '?call =test=> 3) ?arrow folded-fake?)

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
;  '(f 1 (some-macro 33))  =not=> folded-fake?
  )


;; unfolding prerequisites
(facts "about each step of unfolding"
  "unfolding a non-fake just moves the head of the list"
  (unfolding-step '[...] '[blah]   {}) => [ '[... blah]   [] {} ]
  (unfolding-step '[...] '[(blah)] {}) => [ '[... (blah)] [] {} ]

  "unfolding a plain fake does nothing in particular"
  (unfolding-step '[...] `[(fake (f 1) =test=> 4) ...] {})
  => [ `[... (fake (f 1) =test=> 4)] '[...] {} ]

  "unfolding a fake that should be unfolded adds a new fake"
  (let [original (cons `fake '((f (h 1)) =test=> 4 ...overrides...))
        flattened (cons `fake '((f ...h-1...) =test=> 4 ...overrides...))
        generated (cons `fake '((h 1) => ...h-1... ...overrides...))]
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
  (augment-substitutions {'(h 1) ...h-1...} '(fake (#'f (h 1)))) => '{ (h 1) ...h-1... })

(fact "fakes are flattened by making substitutions"
  (flatten-fake '(fake (f (g 1) 2 (h 3)) =test=> 33 ...overrides...)
                '{ (g 1) ...g-1..., (h 3) ...h-1... })
  => '(fake (f ...g-1... 2 ...h-1...) =test=> 33 ...overrides...))

(fact "generated fakes maintain overrrides"
  (let [g-fake `(fake (g 1) => ...g-1... ...overrides...)
        h-fake `(fake (#'h 3) => ...h-1... ...overrides...)]
    (set (generate-fakes `{ (g 1) ...g-1..., (#'h 3) ...h-1... } '(...overrides...)))
    => #{g-fake h-fake}))

