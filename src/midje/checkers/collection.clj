;; -*- indent-tabs-mode: nil -*-

;; Note: checkers need to be exported in ../checkers.clj

(ns midje.checkers.collection
  (:use [clojure.set :only [union]]
        [midje.util.old-clojure-contrib.seq :only [rotations]]
        [midje.util.old-clojure-contrib.def :only [defmacro- defvar-]]
        [clojure.pprint :only [cl-format]]
        [clojure.math.combinatorics :only [permutations]]
        [midje.util.form-utils :only [regex? tack-on-to record? classic-map?]]
        [midje.util.object-utils :only [function-name named-function?]]
	[midje.checkers util extended-equality chatty defining]
        [midje.util.exceptions :only [user-error]]
        [clojure.string :only [join]]))

(def looseness-modifiers #{:in-any-order :gaps-ok})

(defn- inexact-checker? 
  "Can the checker potentially match non-unique elements
   in a seq? (Ex: regex #'a+' can match 'a' and 'aa'.)"
  [checker]
  (or (extended-fn? checker)
      (regex? checker)))

(defn- feasible-permutations 
  "Permute the given list if it contains inexact checkers.
   Only produces all permutations for short lists."
  [checkers]
  (cond (not-any? inexact-checker? checkers)
        [checkers]

        (<= (count checkers) 4)
        (permutations checkers)

        :else
        (rotations checkers)))

(defn- total-match?
  "Have all the expected elements have been discovered?"
  [comparison]
  (= (count (:expected-found comparison))
     (count (:expected comparison))))

(defn- closer-match? 
  "Did the candidate match more expected elements than before?"
  [candidate best-so-far]
  (> (count (:actual-found candidate))
     (count (:actual-found best-so-far))))

(defn- better-of [candidate best-so-far]
  (if (closer-match? candidate best-so-far) candidate best-so-far))

(defn- collection-like?
  "Extend coll? to include strings."
  [thing]
  (or (coll? thing)
      (string? thing)))

(defn- right-hand-singleton? 
  "The kind of thing that, in (contains X), means (contains [X])"
  [thing]
  (or (not (coll? thing)) (map? thing)))

(defn- same-lengths? [actual expected]
  (= (count actual) (count expected)))

(defn- expected-fits? 
  "Could expected fit as a subsequence of actual?"
  [actual expected]
  (>= (count actual) (count expected)))

(defn- noted-falsehood
  "Produce a partially constructed chatty falsehood that contains
   a :notes key with the strings."
  [& strings ]
  (as-chatty-falsehood {:notes strings}))

(defn- try-re 
  "Use the function (re-find or re-matches) to apply re to the thing.
   If function blows up, return a chatty failure about it."
  [re thing function]
  (try
    (function re thing)
    (catch Exception ex
      (noted-falsehood (format "%s can't be used on %s, a %s."
                               (pr-str re) (pr-str thing) (type thing) ".")))))

(defn- base-starting-candidate
  "A data structure that represents which actual elements, matching
   expected elements, have been found from an original set of expected
   elements."
  [expected]
  {:actual-found [], :expected-found [], :expected expected })


;; There is an annoying only-semi-similarity between maps and sequences.
;; These are the generic functions.

(defn- midje-classification [thing] (if (map? thing) ::map ::not-map))

(defmulti #^{:private true} collection-string
  "Given a list of stringified elements, convert them into appropriate
   collection text."
  (fn [midje-classification elements] midje-classification))

(defmethod collection-string ::map [midje-classification elements]
   (str "{" (join ", " (sort elements)) "}"))

(defmethod collection-string ::not-map [midje-classification elements]
   (str "[" (join " " elements) "]"))
;;-

(defmulti #^{:private true} best-actual-match
  "Describe the best actuals found in the comparison."
  (fn [midje-classification comparison] midje-classification))

(defmethod best-actual-match ::not-map [midje-classification comparison]
  (str "Best match found: " (pr-str (:actual-found comparison))))

(defmethod best-actual-match ::map [midje-classification comparison]
  (str "Best match found: {"
       (join ", "
             (sort (for [[k v] (:actual-found comparison)] 
                     (str (pr-str k) " " (pr-str v)))))
    "}."))
;;-


(defmulti #^{:private true} best-expected-match
  "Describe the best list of expected values found in the comparison."
  (fn [midje-classification comparison expected] midje-classification))

(defn- best-expected-match-wrapper
  [midje-classification comparison expected element-maker suffix]
  (if (not-any? inexact-checker? expected)
    nil
    [(str "      It matched: "
          (collection-string midje-classification
                             (map element-maker
                                  (:expected-found comparison)))
          suffix
          ".")]))

(defmethod best-expected-match ::not-map [midje-classification comparison expected]
   (best-expected-match-wrapper midje-classification
                                comparison
                                expected
                                #(cond (named-function? %)
                                       (function-name %)

                                       :else
                                       (pr-str %))
                                " (in that order)"))
    
(defmethod best-expected-match ::map [midje-classification comparison expected]
   (best-expected-match-wrapper midje-classification
                                comparison
                                (vals expected)
                                (fn [[k v]]
                                  (if (named-function? v)
                                    (str (pr-str k) " " (function-name v))
                                    (str (pr-str k) " " (pr-str v))))
                                ""))

;;-

(defmulti #^{:private true} compare-results
  (fn [actual expected looseness]
    (if (= ::map (midje-classification actual))
      (midje-classification actual)
      [::not-map (or (some #{:in-any-order} looseness) :strict-order)])))

;; There are some incommensurable utility behaviors
(defn- compare-one-map-permutation [actual expected keys]
  ;;  (prn "map-comparison" actual expected)
  (reduce (fn [so-far key]
            (if (and (find actual key)
                     (extended-= (get actual key) (get expected key)))
              (merge-with merge so-far {:actual-found {key (get actual key)}
                                        :expected-found {key (get expected key)} })
              so-far))
          {:actual-found {} :expected-found {} :expected expected}
          keys))


(defn- compare-one-seq-permutation
  "Compare actual elements to expected, which is one of perhaps many
   permutations of the original expected list. looseness is a subset of
   #{:gaps-ok :in-any-order}."
  [actual expected looseness]
  (let [starting-candidate (assoc (base-starting-candidate expected) :expected-skipped-over [])
        gaps-ok? (some #{:gaps-ok} looseness)]
    (loop [walking-actual   actual
           walking-expected expected
           best-so-far      starting-candidate
           candidate        starting-candidate]

      ;; (prn "walking actual" walking-actual "walking expected" walking-expected)
      (cond (or (empty? walking-actual) (empty? walking-expected))
            (better-of candidate best-so-far)

            (extended-= (first walking-actual) (first walking-expected))
            ;; A palpable hit! Try any remainder.
            (recur (rest walking-actual)
                   (concat (:expected-skipped-over candidate) (rest walking-expected))
                   best-so-far
                   (merge 
                    (tack-on-to candidate
                                :actual-found (first walking-actual)
                                :expected-found (first walking-expected))
                    {:expected-skipped-over []}))

            (not (empty? (rest walking-expected)))
            ;; Perhaps the next expected element will work. We can, after all, be in any order.
            (recur walking-actual
                   (rest walking-expected)
                   best-so-far
                   (tack-on-to candidate
                               :expected-skipped-over (first walking-expected)))

            (not (empty? (rest walking-actual)))
            ;; OK, there's no match for this actual element in the whole expected.
            (if gaps-ok?
              ;; Since gaps are OK, we can drop the bad actual element and check the next one.
              (recur (rest walking-actual)
                     (concat (:expected-skipped-over candidate) walking-expected)
                     (better-of candidate best-so-far)
                     (merge candidate {:expected-skipped-over []}))

              ;; This actual is blown. Try the next one.
              (recur (rest (concat (:actual-found candidate) walking-actual))
                   expected
                   (better-of candidate best-so-far)
                   starting-candidate))

            :else 
            (better-of candidate best-so-far)))))

(defn- order-free-compare-results [expected expected-permutations try-permutation]
  (loop [expected-permutations expected-permutations
         best-so-far (base-starting-candidate expected)]
    (if (empty? expected-permutations)
      best-so-far
      (let [comparison (try-permutation (first expected-permutations))]
        (if (total-match? comparison)
          comparison
          (recur (rest expected-permutations)
                 (better-of comparison best-so-far)))))))

(defmethod compare-results ::map [actual expected looseness]
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
        gaps-ok? (some #{:gaps-ok} looseness)]

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
                   (tack-on-to candidate
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
                   starting-candidate)))))

;; Initial argument processing

(defn- compatibility-check [actual expected looseness]
  "Fling an error of the combination of actual, expected, and looseness won't work."
  ;; Throwing Errors is just an implementation convenience.
  (cond (regex? expected)
	(cond (and (not (sequential? actual))
		   (not (empty? looseness)))
	      (throw (user-error (str "I don't know how to make sense of a "
                                      "regular expression applied "
                                      looseness "."))))

	(not (collection-like? actual))
	(throw (user-error (str "You can't compare " (pr-str actual) " (" (type actual) 
                                ") to " (pr-str expected) " (" (type expected) ").")))

        (and (record? expected)
             (map? actual)
             (not (= (class expected) (class actual))))
        (throw (user-error (str "You expected a " (.getName (class expected))
                                " but the actual value was a "
                                (if (classic-map? actual) "map" (.getName (class actual)))
                                ".")))
        
	(and (map? actual)
	     (not (map? expected)))
	(try (into {} expected)
	     (catch Throwable ex
	       (throw (user-error (str "If " (pr-str actual) " is a map, "
                                       (pr-str expected)
                                       " should look like map entries.")))))))

  
(defn- standardized-arguments [actual expected looseness]
  "Reduce arguments to standard forms so there are fewer combinations to
   consider. Also blow up for some incompatible forms."

  (compatibility-check actual expected looseness)
  (cond (sequential? actual)
	(cond (set? expected)
	      [actual (vec expected) (union looseness #{:in-any-order})]
	      
	      (right-hand-singleton? expected)
	      [actual [expected] (union looseness #{:in-any-order})]

	      :else
	      [actual expected looseness])

	(map? actual)
	(cond (map? expected)
	      [actual expected looseness]

	      :else
	      [actual (into {} expected) looseness])
	
	(set? actual)
	(recur (vec actual) expected looseness-modifiers)
	
	(string? actual)
	(cond (and (not (string? expected))
		   (not (regex? expected)))
	      (recur (vec actual) expected looseness)
	      :else
	      [ actual expected looseness])

	:else
	[actual expected looseness]))

;;


(defn- match? [actual expected looseness]
  (let [comparison (compare-results  actual expected looseness)]
    (or (total-match? comparison)
        (apply noted-falsehood
               (cons (best-actual-match (midje-classification actual) comparison)
                     (best-expected-match (midje-classification actual) comparison expected))))))

;; The interface

(defn- separate-looseness
  "Distinguish expected results from looseness descriptions.
   1 :in-any-order => [1 [:in-any-order]]
   1 2 :in-any-order => [ [1 2] [:in-any-order] ]
   Single elements require specialized processing, so they're left alone.
   More than one element must be an indication that a sequential is desired."
  [args]
  (let [special-case-singletons #(if (= 1 (count %)) (first %) %)
        past-end-of-real-arguments #(or (empty? %)
                                        (some looseness-modifiers [(first %)]))]
  (loop [known-to-be-expected [(first args)]
         might-be-looseness-modifier (rest args)]
    (if (past-end-of-real-arguments might-be-looseness-modifier)
      (vector (special-case-singletons known-to-be-expected) might-be-looseness-modifier)
      (recur (conj known-to-be-expected (first might-be-looseness-modifier))
             (rest might-be-looseness-modifier))))))

(defn- container-checker-maker [name checker-fn]
   (checker [& args]
     (let [ [expected looseness] (separate-looseness args)]
       (as-chatty-checker
        (named-as-call name expected
               (fn [actual]
                 (add-actual actual
                             (try (checker-fn actual expected looseness)
                                  (catch Error ex
                                    (noted-falsehood (.getMessage ex)))))))))))

(def ^{:midje/checker true} contains (container-checker-maker 'contains
    (fn [actual expected looseness]
      (let [ [actual expected looseness] (standardized-arguments actual expected looseness)]
        (cond (regex? expected)
              (try-re expected actual re-find)
                 
              :else
              (match? actual expected looseness))))))

(def ^{:midje/checker true} just (container-checker-maker 'just
    (fn [actual expected looseness]
      (let [ [actual expected looseness] (standardized-arguments actual expected looseness)]
        (cond (regex? expected)
              (try-re expected actual re-matches)
            
              (same-lengths? actual expected)
              (match? actual expected looseness)

              :else
              (noted-falsehood
               (cl-format nil "Expected ~R element~:P. There ~[were~;was~:;were~]~:* ~R."
                          (count expected)
                          (count actual))))))))

(defn- has-xfix [x-name pattern-fn take-fn]
  (checker [actual expected looseness]
    (cond (set? actual)
          (noted-falsehood (format "Sets don't have %ses." x-name))

          (map? actual)
          (noted-falsehood (format "Maps don't have %ses." x-name))
          
          :else
          (let [ [actual expected looseness] (standardized-arguments actual expected looseness)]
            (cond (regex? expected)
                  (try-re (pattern-fn expected) actual re-find)
                  
                  (expected-fits? actual expected)
                  (match?(take-fn (count expected) actual) expected looseness)

                  :else
                  (noted-falsehood
                   (cl-format nil
                              "A collection with ~R element~:P cannot match a ~A of size ~R."
                              (count actual) x-name (count expected))))))))

(def ^{:midje/checker true} has-prefix
     (container-checker-maker 'has-prefix
      (has-xfix "prefix" #(re-pattern (str "^" (.toString %))) take)))
(def ^{:midje/checker true} has-suffix
     (container-checker-maker 'has-suffix
      (has-xfix "suffix" #(re-pattern (str (.toString %) "$" )) take-last)))
                            
(defchecker has [quantifier predicate]
  (checker [actual]
    (quantifier predicate
                (if (map? actual)
                  (vals actual)
                  actual))))

;; These are used in some internal tests. Worth publicizing?

(defchecker n-of [expected expected-count]
  (chatty-checker [actual]
    (and (= (count actual) expected-count)
         (every? #(extended-= % expected) actual))))


(defmacro- of-functions []
  (let [names {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven",
               8 "eight", 9 "nine", 10 "ten"}
        defns (for [key (keys names)] 
                `(defchecker ~(symbol (str (get names key) "-of")) [expected-checker#]
                   (n-of expected-checker# ~key)))]
    `(do ~@defns)))

(of-functions)