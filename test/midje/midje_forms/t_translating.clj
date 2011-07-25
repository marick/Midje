;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-translating
  (:use [midje.midje-forms.translating])
  (:use [midje.sweet])
  (:use [midje.util.wrapping :only [for-wrapping-target?]])
  (:use midje.test-util)
  (:use [midje.metaconstants
         :only [metaconstant-for-form with-fresh-generated-metaconstant-names]])
  (:use [midje.util thread-safe-var-nesting unify])
  (:require [clojure.zip :as zip])
  (:use clojure.contrib.pprint)
  (:use [ordered.map :only (ordered-map)]))

(testable-privates midje.midje-forms.translating
                   canonicalize-raw-wrappers final-state-wrapper)


(fact "a whole form can have line numbers added to its arrow sequences"
  (let [original `(let ~(with-meta '[a 1] {:line 33})
                    a => 2
                    ~(with-meta '(f 2) {:line 35}) => a)
        actual (add-line-numbers original)
        expected '(clojure.core/let [a 1]
                                    midje.midje-forms.t-translating/a midje.sweet/=> 2 :position (midje.util.file-position/line-number-known 34)
                                    (f 2) midje.sweet/=> midje.midje-forms.t-translating/a :position (midje.util.file-position/line-number-known 35))]
    actual => expected))

(fact "various arrow forms have line numbers"
  (let [original `(
                    (~(with-meta '(f 1) {:line 33}) => 2)
                    (~(with-meta '(f 1) {:line 33}) =not=> 2)
                    (~(with-meta '(f 1) {:line 33}) =streams=> 2)
                    (~(with-meta '(f 1) {:line 33}) =future=> 2))
        actual (add-line-numbers original)]
    (doseq [expansion actual]
      (take-last 2 expansion)
      => '(:position (midje.util.file-position/line-number-known 33)))))

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


(defn lineno
  ([tree] (get (meta tree) :line :not-found))
  ([tree n] (get (meta (nth tree n)) :line :not-found)))

(fact "metadata can be copied from one tree to a matching tree"
  (let [line-number-source '(This has
                      (some line numbers)
                      (on it))
        form-source '(The line  
                      (numbers of this)
                      (tree differ))
        result (form-with-copied-line-numbers form-source line-number-source)]

    line-number-source =not=> form-source
    result => form-source
    
    (lineno form-source) =not=> (lineno line-number-source)
    (lineno form-source 2) =not=> (lineno line-number-source 2)
    (lineno form-source 3) =not=> (lineno line-number-source 3)

    (lineno result) => (lineno line-number-source)
    (lineno result 2) => (lineno line-number-source 2)
    (lineno result 3) => (lineno line-number-source 3)))

(fact "The metadata tree might have nodes where the other tree has branches"
  (let [line-number-source '(This
                 ?1
                 (that)
                 ?2)
        form-source '(This
                      (something (deeply (nested)))
                      (that)
                      (something (deeply (nested))))
        result (form-with-copied-line-numbers form-source line-number-source)]

    line-number-source =not=> form-source
    result => form-source

    (lineno line-number-source 1) => :not-found
    (lineno line-number-source 3) => :not-found
    (lineno form-source 1) =not=> nil
    (lineno form-source 3) =not=> nil
    (lineno result 1) => :not-found
    (lineno result 3) => :not-found

    (lineno result) =not=> (lineno form-source)
    (lineno result) => (lineno line-number-source)

    (lineno result 2) =not=> (lineno form-source 2)
    (lineno result 2) => (lineno line-number-source 2)))

(fact "other metadata is left alone"
  (let [line-number-source '(This (that))
        form-source `(This
                      ~(with-meta
                         '(something (deeply (nested)))
                         {:meta :data, :line 33}))
        result (form-with-copied-line-numbers form-source line-number-source)]
    (lineno result 1) => (lineno line-number-source 1)
    (:meta (meta (nth result 1))) => :data))



(tabular (fact ?comment
           (let [line-no-free-original ?original
                 line-no-free-expected ?expected]
             (add-one-binding-note line-no-free-original (ordered-map '?a 'a))
             => line-no-free-expected))

         ?comment ?original ?expected

         "binding notes can be inserted"
         '(do (midje.semi-sweet/expect (a) => b)
              (do (midje.semi-sweet/expect (inc a) => M)))
         '(do (midje.semi-sweet/expect (a) => b
                                       :binding-note "{?a a}")
                      (do (midje.semi-sweet/expect (inc a) => M
                                                   :binding-note "{?a a}")))

         "fakes do not get insertions"
         '(do (midje.semi-sweet/expect (a) => b
                                       (midje.semi-sweet/fake (x) => 3)))
         '(do (midje.semi-sweet/expect (a) => b :binding-note "{?a a}"
                                       (midje.semi-sweet/fake (x) => 3)))

         "other annotations are preserved"
         '(do (midje.semi-sweet/expect (a) => b :line 33))
         '(do (midje.semi-sweet/expect (a) => b :binding-note "{?a a}" :line 33)))

(fact "binding notes are in the order of the original row - this order is maintained within the ordered-binding-map"
  (let [actual (add-one-binding-note '(do (expect 1 => 2))
                                          (ordered-map '?a 1, '?b 2, '?delta "0", '?result 3))
        expected '(do (expect 1 => 2 :binding-note "{?a 1, ?b 2, ?delta \"0\", ?result 3}"))]
    actual => expected))
    
