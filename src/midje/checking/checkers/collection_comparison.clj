(ns ^{:doc "Code to use to compare collections."}
  midje.checking.checkers.collection-comparison
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]
            [midje.checking.core :refer :all]
            [midje.checking.checkers
             [collection-util :refer :all]
             [util :refer :all]
             [chatty :refer :all]
             [defining :refer :all]]
            [midje.util.pile :as pile]
            [such.maps :as map]))

;; Note: checkers need to be exported in ../checkers.clj

;; There is an annoying only-semi-similarity between maps and sequences.
;; These are the generic functions.

(defn midje-classification [x] (if (map? x) ::map ::not-map))

(defmulti ^{:private true} collection-string
  "Given a list of stringified elements, convert them into appropriate
   collection text."
  (fn [midje-classification _elements_] midje-classification))

(defmethod collection-string ::map [_midje-classification_ elements]
  (str "{" (str/join ", " (sort elements)) "}"))

(defmethod collection-string ::not-map [_midje-classification_ elements]
  (str "[" (str/join " " elements) "]"))

(defmulti best-actual-match
  "Describe the best actuals found in the comparison."
  (fn [midje-classification _comparison_] midje-classification))

(defmethod best-actual-match ::not-map [_midje-classification_ comparison]
  (str "Best match found: " (pr-str (:actual-found comparison))))

(defmethod best-actual-match ::map [_midje-classification_ comparison]
  (str "Best match found: " (pr-str (pile/sort-map (:actual-found comparison)))))

(defmulti best-expected-match
  "Describe the best list of expected values found in the comparison."
  (fn [midje-classification _comparison_ _expected_] midje-classification))

(letfn [(best-expected-match-wrapper
  [midje-classification comparison expected element-maker suffix]
  (when (some inexact-checker? expected)
    [(str "      It matched: "
       (->> comparison :expected-found (map element-maker) (collection-string midje-classification))
       suffix
       ".")]))]

  (defmethod best-expected-match ::not-map [midje-classification comparison expected]
    (best-expected-match-wrapper midje-classification
      comparison
      expected
      (fn [item]
        (if (pile/named-function? item)
          (pile/function-name item)
          (pr-str item)))
      " (in that order)"))

  (defmethod best-expected-match ::map [midje-classification comparison expected]
    (best-expected-match-wrapper midje-classification
      comparison
      (vals expected)
      (fn [[k v]]
        (if (pile/named-function? v)
          (str (pr-str k) " " (pile/function-name v))
          (str (pr-str k) " " (pr-str v))))
      "")))


(letfn [(compare-one-map-permutation
          ;; There are some incommensurable utility behaviors
          [actual expected keys]
          (apply merge-with merge
            { :actual-found {} :expected-found {} :expected expected }
            (for [k keys
                  :when (and (find actual k)
                             (extended-= (get actual k) (get expected k)))]
              {:actual-found {k (get actual k)}
               :expected-found {k (get expected k)}})))

        (base-starting-candidate
          ;; A data structure that represents which actual elements, matching
          ;; expected elements, have been found from an original set of expected
          ;; elements.
          [expected]
          {:actual-found [], :expected-found [], :expected expected})

        (compare-one-seq-permutation
          ;; Compare actual elements to expected, which is one of perhaps many
          ;; permutations of the original expected list. looseness is a subset of
          ;; #{:gaps-ok :in-any-order}."
          [actual expected looseness]
          (let [starting-candidate (assoc (base-starting-candidate expected) :expected-skipped-over [])
                gaps-ok? (some (partial = :gaps-ok) looseness)]
            (loop [walking-actual   actual
                   walking-expected expected
                   best-so-far      starting-candidate
                   candidate        starting-candidate]

              (cond (or (empty? walking-actual) (empty? walking-expected))
                (better-of candidate best-so-far)

                (extended-= (first walking-actual) (first walking-expected))
                ;; A palpable hit! Try any remainder.
                (recur (rest walking-actual)
                  (concat (:expected-skipped-over candidate) (rest walking-expected))
                  best-so-far
                  (merge
                    (map/conj-into candidate
                      :actual-found (first walking-actual)
                      :expected-found (first walking-expected))
                    {:expected-skipped-over []}))

                (not (empty? (rest walking-expected)))
                ;; Perhaps the next expected element will work. We can, after all, be in any order.
                (recur walking-actual
                  (rest walking-expected)
                  best-so-far
                  (map/conj-into candidate
                    :expected-skipped-over (first walking-expected)))

                (not (empty? (rest walking-actual)))
                ;; OK, there's no match for this actual element in the whole expected.
                (if gaps-ok?
                  ;; Since gaps are OK, we can drop the bad actual element and check the next one.
                  (recur (rest walking-actual)
                    (concat (:expected-skipped-over candidate) walking-expected)
                    (better-of candidate best-so-far)
                    (assoc candidate :expected-skipped-over []))

                  ;; This actual is blown. Try the next one.
                  (recur (rest (concat (:actual-found candidate) walking-actual))
                    expected
                    (better-of candidate best-so-far)
                    starting-candidate))

                :else
                (better-of candidate best-so-far)))))

        (order-free-compare-results [expected expected-permutations try-permutation]
          (loop [expected-permutations expected-permutations
                 best-so-far (base-starting-candidate expected)]
            (if (empty? expected-permutations)
              best-so-far
              (let [comparison (try-permutation (first expected-permutations))]
                (if (total-match? comparison)
                  comparison
                  (recur (rest expected-permutations)
                    (better-of comparison best-so-far)))))))

        (feasible-permutations
          ;; "Permute the given list if it contains inexact checkers.
          ;;  Only produces all permutations for short lists."
          [checkers]
          (cond (not-any? inexact-checker? checkers)
            [checkers]

            (<= (count checkers) 4)
            (comb/permutations checkers)

            :else
            (pile/rotations checkers)))]

  (defmulti compare-results
    (fn [actual _expected_ looseness]
      (if (= ::map (midje-classification actual))
        ::map
        [::not-map (or (some #{:in-any-order} looseness) :strict-order)])))

  (defmethod compare-results ::map [actual expected _looseness_]
    (order-free-compare-results expected
      (feasible-permutations (keys expected))
      (fn [permutation]
        (compare-one-map-permutation actual
          expected
          permutation))))

  (defmethod compare-results [::not-map :in-any-order]
    [actual expected looseness]
    (order-free-compare-results expected
      (feasible-permutations expected)
      (fn [permutation]
        (compare-one-seq-permutation actual
          permutation
          looseness))))

  (defmethod compare-results [::not-map :strict-order]
    [actual expected looseness]
    (let [starting-candidate (base-starting-candidate expected)
          gaps-ok? (some (partial = :gaps-ok) looseness)]

      ;; This embeds two loops. walking-actual controls the inner loop. It walks
      ;; until success or it hits a mismatch. actual controls the outer loop.
      ;; Upon each mismatch, it tries again with the #'rest of itself.
      (loop [actual           actual
             walking-actual   actual
             walking-expected expected
             best-so-far      starting-candidate
             candidate        starting-candidate]

        (cond (or (empty? walking-actual) (empty? walking-expected))
          (better-of candidate best-so-far)

          (extended-= (first walking-actual) (first walking-expected))
          ;; actual good so far, keep working on it
          (recur actual
            (rest walking-actual)
            (rest walking-expected)
            best-so-far
            (map/conj-into candidate
              :actual-found (first walking-actual)
              :expected-found (first walking-expected)))

          (and gaps-ok? (not (empty? (rest walking-actual))))
          ;; This is a gap in the walking actual. Skip it.
          (recur actual
            (rest walking-actual)
            walking-expected
            best-so-far
            candidate)

          (not (empty? actual))
          ;; See if we can find something better later on.
          (recur (rest actual)
            (rest actual)
            expected
            (better-of candidate best-so-far)
            starting-candidate))))))
