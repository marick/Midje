(ns midje.data.t-prerequisite-state
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.data.prerequisite-state :refer [binding-map implements-a-fake? usable-default-function?] :as prereq-state]
            [midje.test-util :refer :all]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.parsing.2-to-lexical-maps.data-fakes :refer [data-fake]]
            [midje.util :refer :all]
            [midje.config :as config]
            [such.shorthand :as shorthand]
            [such.sequences :as seq]))

(expose-testables midje.data.prerequisite-state)


(declare f g)

(tabular
  (fact "binding maps contain functions that increment a call count"
    (let [fake (fake (?function-reference 1) => 3)
          result-map (binding-map [fake])]
      ( (result-map #'f) 1) => 3
      @(:call-count-atom fake) => 1))
  ?function-reference
         f
        #'f
        )

(fact "binding maps can also contain Metaconstants to assign"
  (let [data-fakes [(data-fake ...mc... =contains=> {:a 1, :b ...even...})
                    (data-fake ...mc... =contains=> {:c inc})]
        result-map (binding-map data-fakes)]
    (.storage (result-map #'...mc...)) => {:a 1, :b ...even..., :c inc}))

(fact "Unintuitively, earlier binding maps override later"
  (let [fakes [(fake (f 1) => 3 :type :background)
               (fake (f 1) => 4 :type :background)]
        result-map (binding-map fakes)]

    ( (result-map #'f) 1) => 3
    (map #(deref (:call-count-atom %)) fakes) => [1 0]))

(defn called-because-mock-checking-requires-it [] nil)
(defn has-faked-function []
  (called-because-mock-checking-requires-it)
  (implements-a-fake? called-because-mock-checking-requires-it))

(fact "A faked function can be identified from its metadata"
  (has-faked-function) => falsey
  (has-faked-function) => truthy
  (provided
    (called-because-mock-checking-requires-it) => 33))

;;; Handling of default values for fakes


;; Same as above, but using the new config variable

(config/with-augmented-config {:partial-prerequisites true}

  ;; In this example, one call to `internal` is faked and one is left alone.

  (defn internal [x] 33)
  (defn external [x] (+ (internal x) (internal (inc x))))

  (fact "calls not mentioned in prerequisites are passed through to real code"
    (external 1) => 0
    (provided
      (internal 1) => -33))


  ;; The same thing can be done with clojure.core functions

  (defn double-partition [first-seq second-seq]
    (concat (partition-all 1 first-seq) (partition-all 1 second-seq)))

  (fact (double-partition [1 2] [3 4]) => [ [1] [2] [3] [4] ])

  (fact
    (double-partition [1 2] ..xs..) => [[1] [2] [..x1..] [..x2..]]
    (provided (partition-all 1 ..xs..) => [ [..x1..] [..x2..] ]))


  ;; However you can't override functions that are used by Midje itself
  ;; These are reported thusly:

  (defn message-about-mocking-midje-functions [reported]
    (let [important-error
          (shorthand/find-first #(= (:type %) :actual-result-did-not-match-checker)
                                reported)]
      (and important-error
           (.getMessage (.throwable (:actual important-error))))))

  (defn all-even? [xs] (every? even? xs))


  (silent-fact "get a user error from nested call to faked `every?`"
     (all-even? ..xs..) => truthy
     (provided (every? even? ..xs..) => true))
  (fact
    (let [text (message-about-mocking-midje-functions @silent-fact:raw-failures)]
      text => #"seem to have created a prerequisite"
      text => #"clojure\.core/every\?"
      text => #"interferes with.*Midje"))


;; ;; How it works

(defn #^:dynamic function-symbol-of-interest [n] n)
(defn other-function-symbol [])

(fact "best-call-action returns nil [failure], fake [to get value], or default-function"
  (let [matching-fake (fake (function-symbol-of-interest 3) => 4)]
    (best-call-action #'function-symbol-of-interest [3] [matching-fake]) => matching-fake

    (best-call-action #'other-function-symbol [] [matching-fake]) => nil

    (best-call-action #'function-symbol-of-interest [:mismatch] [matching-fake]) => nil
    (provided (usable-default-function? matching-fake) => false)

    (best-call-action #'function-symbol-of-interest [:mismatch] [matching-fake])
    => function-symbol-of-interest
    (provided (usable-default-function? matching-fake) => true)

    ;; This demonstrates that its the default in effect at the time of
    ;; *fake-making* that is used as default, not the value of the function
    ;; (which, after all, is being rebound in the process of mocking).
    (binding [function-symbol-of-interest cons]
      (best-call-action #'function-symbol-of-interest [:mismatch] [matching-fake]))
    => function-symbol-of-interest
    (provided (usable-default-function? matching-fake) => true)))

(facts "When is a var's function (as stashed in fake) usable as a default?"
  (fact "It must have had a value at fake-define time"
    (def var-to-be-fully-faked)
    (usable-default-function? (fake (var-to-be-fully-faked 3) => 1)) => falsey)
  (fact "That value must have been a function."
    (def not-a-function 3)
    (def a-function (fn [x] x))
    (usable-default-function? (fake (not-a-function 3) => 1)) => falsey
    (usable-default-function? (fake (a-function 3) => 1)) => truthy
    (usable-default-function? (fake (#'a-function 3) => 1)) => truthy)
  (fact "It may not have been marked `unfinished`"
    (unfinished tbd)
    (usable-default-function? (fake (tbd 3) => 1)) => falsey
    (usable-default-function? (fake (#'tbd 3) => 1)) => falsey
    ;; However, an unfinished-then-redefined function is allowed
    (unfinished forget-to-remove)
    (def forget-to-remove (fn [x] (+ 3 (* 3 x))))
    (usable-default-function? (fake (forget-to-remove 3) => 1)) => truthy
    (usable-default-function? (fake (#'forget-to-remove 3) => 1)) => truthy)
  (fact "It can be a multimethod"
    (defmulti multimethod type)
    (defmethod multimethod java.lang.String [x] "string me!")
    (usable-default-function? (fake (multimethod 3) => 3)) => truthy))

(defmulti multimethod type)
(defmethod multimethod java.lang.String [x] "string me!")
(fact "fakes can call default functions"
  (handle-mocked-call #'multimethod ["some string"] [(fake (multimethod 4) => 3)])
  => (multimethod "some string"))

(fact "fakes keep track of their call counts"
  (let [fakes [(fake (f 1) => 3)
               (fake (g 1) => 4)
               (fake (#'f 2) => 5)]
        counts (fn []
                 (map #(deref (:call-count-atom %)) fakes))]
    (handle-mocked-call #'f [1] fakes)    (counts) => [1 0 0]
    (handle-mocked-call #'f [1] fakes)    (counts) => [2 0 0]
    (handle-mocked-call #'f [2] fakes)    (counts) => [2 0 1]
    (handle-mocked-call #'g [1] fakes)    (counts) => [2 1 1]))

  )


(def unbound-var)
(def bound-var 3)
(def #^:dynamic rebound)

(config/with-augmented-config {:partial-prerequisites true}

(fact "fakes contain the value of their function-var at moment of binding"
  (:value-at-time-of-faking (fake (unbound-var) => 2)) => nil
  (:value-at-time-of-faking (fake (bound-var) => 888)) => 3
  (:value-at-time-of-faking (fake (#'bound-var) => 888)) => 3
  (binding [rebound 88]
    (:value-at-time-of-faking (fake (rebound) => 3)) => 88))
)

(declare var-for-merged var-for-irrelevant)

(fact "metaconstant bindings can have their values merged together"
  (let [first-half  {:data-fake true :var #'var-for-merged     :contained {:retained 1,   :replaced 2}}
        second-half {:data-fake true :var #'var-for-merged     :contained {:replaced 222, :extra 3}}
        irrelevant  {:data-fake true :var #'var-for-irrelevant :contained {:retained :FOO :extra :BAR}}
        result (binding-map [first-half second-half irrelevant])]
    (.storage (result #'midje.data.t-prerequisite-state/var-for-merged))     => {:retained 1, :replaced 222, :extra 3}
    (.storage (result #'midje.data.t-prerequisite-state/var-for-irrelevant)) => {:retained :FOO, :extra :BAR}))

(unfinished faked-fn)
(facts "fake and datafake maps include form info, so tool creators can introspect them"
  (fake (faked-fn 1 1) => 2 :key :value) => (contains {:call-form '(faked-fn 1 1)
                                                       :arrow '=>
                                                       :rhs (contains [2 :key :value] :gaps-ok)})

  (data-fake ..d.. =contains=> {:key :value}) => (contains {:call-form '..d..
                                                            :arrow '=contains=>
                                                            :rhs (contains [{:key :value}])}))

(fact "data-fakes can be converted to metaconstant-bindings"
  (let [bindings (binding-map [{:data-fake true :var #'name :contained {:a 1}}])
        [_var_ metaconstant] (seq/only bindings)]
    (.underlying-symbol metaconstant) => 'name
    (.storage metaconstant) => {:a 1} ))

(unfinished f)
(fact "Able to mock exact keyword accessor function invocations"
  (f (:fst {:fst 2})) => 2
  (provided
    (f (:fst {:fst 2})) => 2))

(unfinished g)
(defn call-g [] (g (:fst {:fst 2})))
(fact "Able to mock exact keyword accessor invocations in nested function"
  (call-g) => 2
  (provided
    (g (:fst {:fst 2})) => 2))

(def my-inc inc)
(fact "Able to mock exact nested function invocations"
  (f (my-inc 1)) => 2
  (provided
    (f (my-inc 1)) => 2))

(defn multi-arity-func
  ([a] a)
  ([a b] a))

(defn opts-func
  ([a] a)
  ([a b & opts] a))

(defn single-arity-func
  [a] a)
(unfinished unfinished-func)
(declare declared-func)

(fact "check if provided arg count matches one of the function's arglists"
  (#'prereq-state/correct-arg-count? #'single-arity-func 0) => falsey
  (#'prereq-state/correct-arg-count? #'single-arity-func 1) => truthy
  (#'prereq-state/correct-arg-count? #'single-arity-func 2) => falsey

  (#'prereq-state/correct-arg-count? #'multi-arity-func 0) => falsey
  (#'prereq-state/correct-arg-count? #'multi-arity-func 1) => truthy
  (#'prereq-state/correct-arg-count? #'multi-arity-func 2) => truthy
  (#'prereq-state/correct-arg-count? #'multi-arity-func 3) => falsey

  (#'prereq-state/correct-arg-count? #'opts-func 0) => falsey
  (#'prereq-state/correct-arg-count? #'opts-func 1) => truthy
  (#'prereq-state/correct-arg-count? #'opts-func 2) => truthy
  (#'prereq-state/correct-arg-count? #'opts-func 3) => truthy

  (#'prereq-state/correct-arg-count? #'unfinished-func 1) => truthy
  (#'prereq-state/correct-arg-count? #'unfinished-func 0) => truthy

  (#'prereq-state/correct-arg-count? #'declared-func 0) => truthy
  (#'prereq-state/correct-arg-count? #'declared-func 10) => truthy)

;;; DO NOT DELETE
;;; These are used to test the use of vars to fake private functions
;;; in another namespace.

(defn- var-inc [x] (inc x))
(defn- var-inc-user [x] (* x (var-inc x)))
(defn- var-twice []
  (var-inc (var-inc 2)))
