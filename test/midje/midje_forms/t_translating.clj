;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-translating
  (:use [midje.midje-forms.translating]
        [midje.prerequisites :only [mockable-funcall?]])

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


