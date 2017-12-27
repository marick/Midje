(ns ^{:doc "Checkers for collections and strings."}
  midje.checking.checkers.collection
  (:require [such.control-flow :refer [branch-on]]
            [such.types :as types]
            [clojure.pprint :as pprint]
            [midje.checking.core :refer :all]
            [midje.checking.checkers.collection-util :refer :all]
            [midje.checking.checkers.util :refer :all]
            [midje.checking.checkers.chatty :refer :all]
            [midje.checking.checkers.defining :refer :all]
            [midje.checking.checkers.collection-comparison :refer :all]
            [midje.util.exceptions :refer [user-error]]
            [midje.util.pile :as pile]
            [such.sequences :as seq]))

;; Note: checkers need to be exported in ../checkers.clj

(def #^:private looseness-modifiers #{:in-any-order :gaps-ok})

(defn- ^{:testable true} separate-looseness
  "Distinguish expected results from looseness descriptions.
   1 :in-any-order => [1 [:in-any-order]]
   1 2 :in-any-order => [ [1 2] [:in-any-order] ]
   Single elements require specialized processing, so they're left alone.
   More than one element must be an indication that a sequential is desired."
  [args]
  (letfn [(special-case-singletons [x] (if (= 1 (count x)) (first x) x))
          (past-end-of-real-arguments [x] (or (empty? x)
                                              (some looseness-modifiers [(first x)])))]
  (loop [known-to-be-expected [(first args)]
         might-be-looseness-modifier (rest args)]
    (if (past-end-of-real-arguments might-be-looseness-modifier)
      (vector (special-case-singletons known-to-be-expected) might-be-looseness-modifier)
      (recur (conj known-to-be-expected (first might-be-looseness-modifier))
             (rest might-be-looseness-modifier))))))

(defn compatibility-check
  "Fling an error if the combination of actual, expected, and looseness won't work."
  [actual expected looseness]

  ;; Throwing Errors is just an implementation convenience.
  (cond (types/regex? expected)
        (cond (and (not (sequential? actual))
                   (not (empty? looseness)))
              (throw (user-error (str "I don't know how to make sense of a "
                                      "regular expression applied "
                                      looseness "."))))

        (not (collection-like? actual))
        (throw (user-error (str "You can't compare " (pr-str actual) " (" (type actual)
                                ") to " (pr-str expected) " (" (type expected) ").")))

        (inherently-false-map-to-record-comparison? actual expected)
        (throw (user-error (inherently-false-map-to-record-comparison-note actual expected)))

        (and (map? actual)
             (not (map? expected)))
        (try (into {} expected)
             (catch Throwable ex
               (throw (user-error (str "If " (pr-str actual) " is a map, "
                                       (pr-str expected)
                                       " should look like map entries.")))))))

(defn standardized-arguments
  "Reduce arguments to standard forms so there are fewer combinations to
  consider. Also blow up for some incompatible forms."
  [actual expected looseness]
  (compatibility-check actual expected looseness)
  (cond
    (and (sequential? actual) (set? expected))
    [actual (vec expected) (clojure.set/union looseness #{:in-any-order})]

    (and (sequential? actual) (right-hand-singleton? expected))
    [actual [expected] (clojure.set/union looseness #{:in-any-order})]

    (sequential? actual)
    [actual expected looseness]

    (and (map? actual) (map? expected))
    [actual expected looseness]

    (and (map? actual) (= (count (keys (into {} expected))) (count expected)))
    [actual (into {} expected) looseness]

    (set? actual)
    (recur (vec actual) expected looseness-modifiers)

    (and (string? actual)
         (not (string? expected))
         (not (types/regex? expected)))
    (recur (vec actual) expected looseness-modifiers)

    :else
    [actual expected looseness]))

(defn match? [actual expected looseness]
  (let [comparison (compare-results actual expected looseness)]
    (or (total-match? comparison)
        (apply noted-falsehood
               (cons (best-actual-match (midje-classification actual) comparison)
                     (best-expected-match (midje-classification actual) comparison expected))))))

(defn container-checker-maker [name checker-fn]
  (checker [& args]
           (let [[expected looseness] (separate-looseness args)]
             (as-chatty-checker
              (named-as-call name expected
                             (fn [actual]
                               (add-actual actual
                                           (try (checker-fn actual expected looseness)
                                                (catch Error ex
                                                  (noted-falsehood (.getMessage ex)))))))))))

(defn has-xfix [x-name pattern-fn take-fn]
  (checker [actual expected looseness]
           (branch-on actual
                      set? (noted-falsehood (format "Sets don't have %ses." x-name))
                      map? (noted-falsehood (format "Maps don't have %ses." x-name))
                      :else (let [[actual expected looseness] (standardized-arguments actual expected looseness)]
                              (cond (types/regex? expected)
                                    (try-re (pattern-fn expected) actual re-find)

                                    (expected-fits? actual expected)
                                    (match? (take-fn (count expected) actual) expected looseness)

                                    :else (noted-falsehood
                                           (pprint/cl-format nil
                                                             "A collection with ~R element~:P cannot match a ~A of size ~R."
                                                             (count actual) x-name (count expected))))))))

(def ; has-prefix
  ^{:midje/checker true
    :doc "Checks that the actual result starts with the expected result:

  [1 2 3] => (has-prefix   [1 2]) ; true
  [1 2 3] => (has-prefix   [2 1]) ; false - order matters
  [1 2 3] => (has-prefix   [2 1] :in-any-order) ; true
  [1 2 3] => (has-prefix  #{2 1}) ; true "
    :arglists '([expected-prefix]
                  [expected-prefix looseness?])}
  has-prefix
  (container-checker-maker 'has-prefix
                           (has-xfix "prefix" #(re-pattern (str "^" %)) take)))

(def ; has-suffix
  ^{:midje/checker true
    :doc "Checks that the actual result ends with the expected result:

  [1 2 3] => (has-suffix   [2 3]) ; true
  [1 2 3] => (has-suffix   [3 2]) ; false - order matters
  [1 2 3] => (has-suffix   [3 2] :in-any-order) ; true
  [1 2 3] => (has-suffix  #{3 2}) ; true "
    :arglists '([expected-suffix]
                  [expected-suffix looseness?])}
  has-suffix
  (container-checker-maker 'has-suffix
                           (has-xfix "suffix" #(re-pattern (str % "$")) take-last)))


(def ; contains
  ^{:midje/checker true
    :doc "Checks that the expected result is a subsequence of the actual result:

To succeed, f's result must be (1) contiguous and (2) in the
same order as in the contains clause. Here are examples:

   [3 4 5 700]   => (contains [4 5 700]) ; true
   [4 700 5]     => (contains [4 5 700]) ; false
   [4 5 'hi 700] => (contains [4 5 700]) ; false

The :in-any-order modifier loosens the second requirement:

   ['hi 700 5 4] => (contains [4 5 700] :in-any-order) ; true
   [4 5 'hi 700] => (contains [4 5 700] :in-any-order) ; false b/c 'hi is in middle

The :gaps-ok modifier loosens the first:

   [4 5 'hi 700]  => (contains [4 5 700] :gaps-ok) ; true
   [4 700 'hi' 5] => (contains [4 5 700] :gaps-ok) ; false b/c of bad order

The two modifiers can be used at the same time:

   [4 700 5]         => (contains [4 5 700] :gaps-ok :in-any-order) ; true
   [4 5 'hi 700]     => (contains [4 5 700] :in-any-order :gaps-ok) ; true
   [700 'hi 4 5 'hi] => (contains [4 5 700] :in-any-order :gaps-ok) ; true

Another way to indicate :in-any-order is to describe
what's contained by a set. The following two are equivalent:

   [700 4 5] => (contains [4 5 700] :in-any-order)
   [700 4 5] => (contains #{4 5 700})

:gaps-ok can be used with a set. (So can :in-any-order, but it has no effect.)"
    :arglists '([expected]
                  [expected looseness])}
  contains (container-checker-maker 'contains
                                    (fn [actual expected looseness]
                                      (let [[actual expected looseness] (standardized-arguments actual expected looseness)]
                                        (cond (types/regex? expected)
                                              (try-re expected actual re-find)

                                              :else (match? actual expected looseness))))))

(def ; just
  ^{:midje/checker true
    :doc "A variant of contains, just, will fail if the
left-hand-side contains any extra values:
   [1 2 3] => (just [1 2 3])  ; true
   [1 2 3] => (just [1 2 3 4]) ; false

The first of those seems senseless, since you could just use this:

   [1 2 3] => [1 2 3]

However, it's required if you want to use checkers in the expected result:

   [1 2 3] => [odd? even? odd?]  ; false b/c 2nd-level fns aren't normally treated as checkers.
   [1 2 3] => (just [odd? even? odd?]) ; true

just is also useful if you don't care about order:

  [1 3 2] => (just   [1 2 3] :in-any-order)
  [1 3 2] => (just  #{1 2 3})"
    :arglists '([expected]
                [expected looseness])}
  just (container-checker-maker 'just
                                (fn [actual expected looseness]
                                  (let [[actual expected looseness] (standardized-arguments actual expected looseness)]
                                    (cond (types/regex? expected)
                                          (try-re expected actual re-matches)

                                          (same-lengths? actual expected)
                                          (match? actual expected looseness)

                                          :else (noted-falsehood
                                                 (pprint/cl-format nil "Expected ~R element~:P. There ~[were~;was~:;were~]~:* ~R."
                                                                   (count expected)
                                                                   (count actual))))))))

(defchecker has
  "You can apply Clojure's quantification functions (every?, some, and so on)
   to all the values of sequence.

   Ex. (fact (f) => (has every? odd?))"
  [quantifier predicate]
  (checker [actual]
    (let [lifted-quantifier
          (fn [predicate collection]
            (quantifier #(let [predicate-to-run (if (types/regex? predicate)
                                                  (fn [actual] (try-re predicate actual re-find))
                                                  predicate)]
            (extended-true? (predicate-to-run %))) collection))]
      (lifted-quantifier predicate
                  (if (map? actual)
                    (vals actual)
                    actual)))))

(defchecker n-of
  "Checks whether a sequence contains precisely n results, and
   that they each match the checker.

  Ex. (fact (repeat 100 :a) => (n-of :a 100))"
  [expected expected-count]

  (chatty-checker [actual]
    (and (= (count actual) expected-count)
         (every? #(extended-= % expected) actual))))

(defmacro #^:private generate-n-of-checkers []
  (pile/macro-for [[num num-word] [[1 "one"] [2 "two"] [3 "three"] [4 "four"] [5 "five"]
                                   [6 "six"] [7 "seven"] [8 "eight"] [9 "nine"] [10 "ten"]]]
    (let [name (symbol (str num-word "-of"))
          docstring (pprint/cl-format nil "Checks whether a sequence contains precisely ~R result~:[s, and \n  that they each match~;, and \n  that it matches~] the checker.

   Ex. (fact ~A => (~C :a))" num (= num 1) (vec (repeat num :a )) name)]
      `(defchecker
         ~name
         ~docstring
         {:arglists '([~'expected])}
         [expected#]
         (n-of expected# ~num)))))

(generate-n-of-checkers)
