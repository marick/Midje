;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-translating
  (:use [midje.midje-forms.translating])
  (:use [midje.sweet])
  (:use midje.test-util)
  (:use [midje.midje-forms.building
         :only [metaconstant-for-form forgetting-unfolded-prerequisites]])
  (:use [midje.util thread-safe-var-nesting unify])
  (:require [clojure.zip :as zip])
  (:use clojure.contrib.pprint))
(testable-privates midje.midje-forms.translating
                   canonicalize-raw-wrappers final-state-wrapper replace-with-magic-form)




(fact "a whole form can have line numbers added to its arrow sequences"
  (let [original `(let ~(with-meta '[a 1] {:line 33})
                    a => 2
                    ~(with-meta '(f 2) {:line 35}) => a)
        actual (add-line-numbers original)
        expected '(clojure.core/let [a 1]
                                    midje.midje-forms.t-translating/a midje.sweet/=> 2 :position (midje.util.file-position/line-number-known 34)
                                    (f 2) midje.sweet/=> midje.midje-forms.t-translating/a :position (midje.util.file-position/line-number-known 35))]
    actual => expected))




;; Translating sweet forms into their semi-sweet equivalent

(fact "can convert prerequisites into fake calls"
  (let [original '( provided                        (f 1) => 3                         (f 2) => (+ 1 1))
        translated '(        (midje.semi-sweet/fake (f 1) => 3) (midje.semi-sweet/fake (f 2) => (+ 1 1)))
        z (zip/seq-zip original)
        loc (zip/down z)]
    (expand-prerequisites-into-fake-calls loc) => translated))


(fact "translating entire fact forms"
  "some parts of a fact are to be left alone"
  (let [form '(a-form-would-go-here another-would-go-here)]
    (translate-fact-body form) => form)

  (let [form '( (nested (form) form ) [ 1 2 3])]
    (translate-fact-body form) => form)

  "arrow sequences are wrapped with expect"
  (let [form '(                              (f 1)                  => [2]                           (f 2)                  => (+ 1 2) )
        expected '( (midje.semi-sweet/expect (f 1) => [2]) (midje.semi-sweet/expect (f 2) => (+ 1 2)))]
    (expect (translate-fact-body form) => expected))

  "the wrapping can include prerequisites turned into fake forms."
  (let [form '( (f 1) => [1] :ekey "evalue"
                (f 2) => (+ 2 2)
                (provided (g 3) => 3
                          (g 4) => 4 :pkey "pvalue")
                (f 5) => truthy)
        expected '( (midje.semi-sweet/expect (f 1) => [1] :ekey "evalue")
                    (midje.semi-sweet/expect (f 2) => (+ 2 2)
                                             (midje.semi-sweet/fake (g 3) => 3)
                                             (midje.semi-sweet/fake (g 4) => 4 :pkey "pvalue"))
                    (midje.semi-sweet/expect (f 5) => truthy))]
    (translate-fact-body form) => expected)

  "It's useful to embed expect clauses with notcalled prerequisites, so they're skipped"
  (let [form '(    (expect (f 1) => 2 (fake (g 1) => 2))
                                      (fake (m 1) => 33))]
    (translate-fact-body form) => form))

;; wrapping



(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (canonicalize-raw-wrappers []) => []
  (canonicalize-raw-wrappers '[(f 1) => 2]) =>
                                 '[(midje.semi-sweet/fake (f 1) => 2 :type :background)]
  (canonicalize-raw-wrappers '[   (f 1) => 2 :foo 'bar (f 2) => 33 ]) => 
                              '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar :type :background)
                                (midje.semi-sweet/fake (f 2) => 33 :type :background) ]

  "other types are left alone"
  (canonicalize-raw-wrappers
   '[ (before :checks (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checks (swap! test-atom (constantly 0))) ]

 "mixtures"
 (canonicalize-raw-wrappers
   '[ (f 1) => 2 (before :checks (swap! test-atom (constantly 0))) (f 2) => 3 ]) =>
   '[ (midje.semi-sweet/fake (f 1) => 2 :type :background)
      (before :checks (swap! test-atom (constantly 0)))
      (midje.semi-sweet/fake (f 2) => 3 :type :background) ]
 
 "error cases"
 (canonicalize-raw-wrappers '[ (after anything) ]) => (throws Error)
 )

(defn guard-special-form [bindings]
  (assoc (dissoc bindings '?danger) '?danger (str (bindings '?danger))))

(defmacro wrapping-form-is [ original expected ]
  (let [bindings (unify expected (final-state-wrapper original)) ]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" }))

;; The magical symbol that's used in wrapper substitution can't be used in
;; a fact because it gets substituted. So we let the caller use "danger" instead.
(defn- form-matching? [expected]
  (fn [actual] (= actual
                  (subst expected {'?danger 'midje.midje-forms.t-translating/?form}))))

(fact "canonicalized setup/teardown wrappers can be put into final form"
  (let [final (final-state-wrapper '(before :checks (do-something)))]
    final => (form-matching? '(try (do-something) ?danger (finally nil)))
    final => (for-wrapping-target? :checks))

  (let [final (final-state-wrapper '(before :facts (do-something) :after (finish)))]
    final => (form-matching? '(try (do-something) ?danger (finally (finish))))
    final => (for-wrapping-target? :facts))

  (let [final (final-state-wrapper '(after :all (do-something)))]
    final => (form-matching? '(try ?danger (finally (do-something))))
    final => (for-wrapping-target? :all))

  (let [final (final-state-wrapper '(around :checks (let [x 1] ?form)))]
    final => (form-matching? '(let [x 1] ?danger))
    final => (for-wrapping-target? :checks))
)

(facts "about safe expansion of weird forms"
  (map? {1 'do}) => truthy
  (first (second '(midje.semi-sweet.expect (midje.sweet.fact 1 => 2)))) => 'midje.sweet.fact
  (set? #{1 'do}) => truthy)


;; unfolding prerequisites
(facts "about each step of unfolding"
  "unfolding a non-fake just moves the head of the list"
  (unfolding-step '[...] '[blah]   {}) => [ '[... blah]   [] {} ]
  (unfolding-step '[...] '[(blah)] {}) => [ '[... (blah)] [] {} ]

  "unfolding a plain fake does nothing in particular"
  (unfolding-step '[...] '[(midje.semi-sweet/fake (f 1) arrow 4) ...] {})
  => [ '[... (midje.semi-sweet/fake (f 1) arrow 4)] '[...] {} ]

  "unfolding a fake that should be unfolded adds a new fake"
  (let [original '(midje.semi-sweet/fake (f (h 1)) arrow 4 ...overrides...)
        flattened '(midje.semi-sweet/fake (f ...h-1...) arrow 4 ...overrides...)
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

(println "replace arrow with dummy arrow")
(fact "fakes are flattened by making substitutions"
  (flatten-fake '(fake (f (g 1) 2 (h 3)) =test=> 33 ...overrides...)
                '{ (g 1) ...g-1..., (h 3) ...h-1... })
  => '(fake (f ...g-1... 2 ...h-1...) =test=> 33 ...overrides...))

(fact "generated fakes maintain overrrides"
  (let [g-fake '(midje.semi-sweet/fake (g 1) midje.semi-sweet/=> ...g-1... ...overrides...)
        h-fake '(midje.semi-sweet/fake (h 3) midje.semi-sweet/=> ...h-1... ...overrides...)]
    (set (generate-fakes '{ (g 1) ...g-1..., (h 3) ...h-1... } '(...overrides...)))
    => #{g-fake h-fake}))
   
(fact "a fake that needs unfolding has a nested left-hand-side"
  '1                                            =not=> fake-that-needs-unfolding?
  '(fake (f (h 1)))                             =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/non-fake (f (h 1)))        =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1) =test=> 3)      =not=> fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f (h 1)) =test=> 3)  => fake-that-needs-unfolding?
  '(midje.semi-sweet/fake (f 1 (h 1)) =test= 3) => fake-that-needs-unfolding?

  "but don't decide to unfold a checker used as argument matcher"
  '(midje.semi-sweet/fake (f 1 (exactly even?)) =test=> 3) =not=> fake-that-needs-unfolding?
  "Or special forms that look like functions"
  '(midje.semi-sweet/fake (f 1 (quote foo)) =test=> 3) =not=> fake-that-needs-unfolding?
  )

