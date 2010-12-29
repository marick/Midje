;; -*- indent-tabs-mode: nil -*-

(ns midje.util.checkers
  (:use [clojure.set :only [difference union subset?]]
        [clojure.contrib.seq :only [rotations]]
        [clojure.contrib.def :only [defmacro-]]
        [clojure.contrib.pprint :only [cl-format]]
        [clojure.contrib.combinatorics :only [permutations]]
        [midje.util.form-utils :only [regex? vector-without-element-at-index
                                      tack-on-to]]))

(declare chatty-checker-falsehood? captured-exception?)

;; Midje has its own peculiar idea of equality

(defn- extended-fn? [x]
  (or (fn? x)
      (= (class x) clojure.lang.MultiFn)))

(defn extended-= [actual expected]
  (try
    (cond (chatty-checker-falsehood? actual)
          actual

          (chatty-checker-falsehood? expected)
          expected
          
          (extended-fn? expected)
          (let [function-result (expected actual)]
            (if (chatty-checker-falsehood? function-result) false function-result))
        
          (regex? expected)
          (if (regex? actual)
            (= (.toString actual) (.toString expected))
            (re-find expected actual))

          :else
          (= actual expected))
    (catch Exception ex false)))


;; Simple checkers

(defn- named [name expected function]
  "Adds a string name that looks like a function call to
   a functions metadata under :name"
  (with-meta function
        {:name (format "(%s %s)" name expected)}))
  

(defn truthy 
  "Returns precisely true if actual is not nil and not false."
  {:midje/checker true}
  [actual] 
  (and (not (captured-exception? actual))
       (not (not actual))))

(defn falsey 
  "Returns precisely true if actual is nil or false."
  {:midje/checker true}
  [actual] 
  (not actual))

(defn anything
  "Accepts any value"
  {:midje/checker true}
  [actual]
  (not (captured-exception? actual)))
(def irrelevant anything)

(defn exactly
  "Checks for equality. Use to avoid default handling of functions."
  {:midje/checker true}
  [expected]
    (named 'exactly expected
           (fn [actual] (= expected actual))))


;; Chatty checkers

(defn tag-as-chatty-falsehood [value]
  (with-meta value {:midje/chatty-checker-falsehood true}))

(defn- tag-as-checker [function]
  (vary-meta function merge {:midje/checker true}))
  
(defn tag-as-chatty-checker [function]
  (tag-as-checker (vary-meta function merge {:midje/chatty-checker true})))

(defn chatty-falsehood-to-map [value]
  (with-meta value {}))

(defn chatty-checker-falsehood? [value]
  (:midje/chatty-checker-falsehood (meta value)))

(defn chatty-checker? [fn]
  (:midje/chatty-checker (meta fn)))

(defn chatty-worth-reporting-on? [arg]
  (and (list? arg)
       (> (count arg) 0)
       (not (= (first arg) 'clojure.core/quote))))

(defn chatty-untease [result-symbol arglist]
  (loop [ [current-arg & remainder :as arglist] arglist
          complex-forms []
          substituted-arglist []]
    (cond (empty? arglist)
          [complex-forms substituted-arglist]

          (chatty-worth-reporting-on? current-arg)
          (recur remainder (conj complex-forms current-arg)
                 (conj substituted-arglist `(~result-symbol ~(count complex-forms))))

          :else
          (recur remainder complex-forms (conj substituted-arglist current-arg)))))
          
(defmacro chatty-checker
  "Create a function that returns either true or a detailed description of a failure."
  [ [binding-var] [function & arglist] ]
  (let [result-symbol (gensym "chatty-intermediate-results-")
        [complex-forms substituted-arglist] (chatty-untease result-symbol arglist)]
    `(tag-as-chatty-checker
      (fn [~binding-var]
        (let [~result-symbol (vector ~@complex-forms)]
          (if (~function ~@substituted-arglist)
            true
            (let [pairs# (map vector '~complex-forms ~result-symbol)]
              (tag-as-chatty-falsehood {:actual ~binding-var,
                                        :intermediate-results pairs#}))))))))

;;Concerning Throwables

(def #^{:private true} captured-exception-key "this Throwable was captured by midje:")
(defn captured-exception [e] {captured-exception-key e})
(defn captured-exception? [value] (and (map? value) (value captured-exception-key)))

(defn- throwable-with-class? [wrapped-throwable expected-class]
  (and (map? wrapped-throwable)
       (= expected-class (class (wrapped-throwable captured-exception-key)))))

(defn throws
  "Checks that Throwable of named class was thrown and, optionally, that
   the message is as desired."
  {:midje/checker true}
  ([expected-exception-class]
     (fn [wrapped-throwable] (throwable-with-class? wrapped-throwable expected-exception-class)))
  ([expected-exception-class message]
     (fn [wrapped-throwable]
       (and (throwable-with-class? wrapped-throwable expected-exception-class)
            (extended-= (.getMessage (wrapped-throwable captured-exception-key))
                        message)))))

;;Checkers that work with collections.

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
  (tag-as-chatty-falsehood {:notes strings}))

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

;; The code that compares two sequentials.

(defn in-any-order--one-permutation
  "Compare actual elements to expected, which is one of perhaps many
   permutations of the original expected list. kind is a subset of
   #{:gaps-ok :in-any-order}."
  [actual expected kind]
  (let [starting-candidate (assoc (base-starting-candidate expected) :expected-skipped-over [])
        gaps-ok? (some #{:gaps-ok} kind)]
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

(defmulti seq-comparison
  "Compare an actual seq to an expected seq, governed by kind."
  (fn [actual expected kind]
    (or (some #{:in-any-order} kind) :strict-order)))

(defmethod seq-comparison :in-any-order
  [actual expected kind]
  (loop [expected-permutations (feasible-permutations expected)
         best-so-far (base-starting-candidate expected)]
    (if (empty? expected-permutations)
      best-so-far
      (let [comparison (in-any-order--one-permutation actual
                                                      (first expected-permutations)
                                                      kind)]
        (if (total-match? comparison)
          comparison
          (recur (rest expected-permutations)
                 (better-of comparison best-so-far)))))))

(defmethod seq-comparison :strict-order
  [actual expected kind]
  (let [starting-candidate (base-starting-candidate expected)
        gaps-ok? (some #{:gaps-ok} kind)]

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

(defn compatibility-check [actual expected kind]
  "Fling an error of the combination of actual, expected, and kind won't work."
  ;; Throwing Errors is just an implementation convenience.
  (cond (regex? expected)
	(cond (and (not (sequential? actual))
		   (not (empty? kind)))
	      (throw (Error. (str "I don't know how to make sense of a "
				  "regular expression applied "
				  kind "."))))

	(not (collection-like? actual))
	(throw (Error. (str "You can't compare " (pr-str actual) " (" (type actual) 
			    ") to " (pr-str expected) " (" (type expected) ").")))
	
	(and (map? actual)
	     (not (map? expected)))
	(try (into {} expected)
	     (catch Throwable ex
	       (throw (Error. (str "If " (pr-str actual) " is a map, "
				   (pr-str expected)
				   " should look like map entries.")))))))

  
(defn- standardized-arguments [actual expected kind]
  "Reduce arguments to standard forms so there are fewer combinations to
   consider. Also blow up for some incompatible forms."

  (compatibility-check actual expected kind)
  (cond (sequential? actual)
	(cond (set? expected)
	      [actual (vec expected) (union kind #{:in-any-order})]
	      
	      (right-hand-singleton? expected)
	      [actual [expected] (union kind #{:in-any-order})]

	      :else
	      [actual expected kind])

	(map? actual)
	(cond (map? expected)
	      [actual expected kind]

	      :else
	      [actual (into {} expected) kind])
	
	(set? actual)
	(recur (vec actual) expected #{:in-any-order :gaps-ok})
	
	(string? actual)
	(cond (and (not (string? expected))
		   (not (regex? expected)))
	      (recur (vec actual) expected kind)
	      :else
	      [ actual expected kind])

	:else
	[actual expected kind]))

;;

(defn- map-or-not [thing] (if (map? thing) :map :not-map))

(defmulti best-match-actual map-or-not)
(defmethod best-match-actual :not-map [comparison]
  (str "Best match found: " (pr-str (:actual-found comparison))))
(defmethod best-match-actual :map [comparison]
  (str "Best match found: {"
       (apply str
              (interpose ", "
                         (sort (map (fn [[k v]] (str (pr-str k) " " (pr-str v)))
                                    (:actual-found comparison)))))
       "}."))

(defn- best-seq-match-expected [comparison expected]
  (if (or (some extended-fn? expected)
          (some regex? expected))
    (str "      It matched: ["
         (apply str
                (interpose " "
                           (map #(cond (and (fn? %) (:name (meta %)))
                                       (:name (meta %))

                                       :else
                                       (pr-str %))
                                (:expected-found comparison))))
         "] (in that order)")))
    
(defn- best-map-match-expected [comparison expected]
  (if (or (some extended-fn? (vals expected))
          (some regex? (vals expected)))
    (str "      It matched: {"
         (apply str
                (interpose ", "
                           (sort (map (fn [[k v]]
                                        (cond (and (fn? v) (:name (meta v)))
                                              (str (pr-str k) " " (:name (meta v)))
                                              
                                              :else
                                              (str (pr-str k) " " (pr-str v))))
                                      (:expected-found comparison)))))
         "}.")))
    

;; TODO: try different combinations?
(defn map-comparison--one-permutation [actual expected keys]
  ;;  (prn "map-comparison" actual expected)
  (reduce (fn [so-far key]
            ;; (println so-far key)
            (if (and (find actual key)
                     (extended-= (get actual key) (get expected key)))
              (merge-with merge so-far {:actual-found {key (get actual key)}
                                        :expected-found {key (get expected key)} })
              so-far))
          {:actual-found {} :expected-found {} :expected expected}
          keys))

(defn map-comparison [actual expected kind]
  (loop [expected-permutations (feasible-permutations (keys expected))
         best-so-far {:actual-found [], :expected-found [],
                      :expected expected }]
    (if (empty? expected-permutations)
      best-so-far
      (let [comparison (map-comparison--one-permutation actual
                                                        expected
                                                        (first expected-permutations))]
        (if (total-match? comparison)
          comparison
          (recur (rest expected-permutations)
                 (better-of comparison best-so-far)))))))


(defn map-match? [actual expected kind]
;  (println 'map-match actual expected kind)
  (let [comparison (map-comparison actual expected kind)
        mismatch-description (fn [comparison expected]
                               (remove nil? [ (best-match-actual comparison)
                                              (best-map-match-expected comparison expected) ]))]
    (or (total-match? comparison)
        (tag-as-chatty-falsehood
         {
          :notes (mismatch-description comparison expected)}))))

(defn full-sequence-match? [actual expected kind]
  (let [comparison (seq-comparison actual expected kind)
        mismatch-description (fn [comparison expected]
                               (remove nil? [ (best-match-actual comparison)
                                              (best-seq-match-expected comparison expected) ]))]
    (or (total-match? comparison)
        (tag-as-chatty-falsehood
         {
          :notes (mismatch-description comparison expected) }))))

(defn- actual-x-contains? [actual expected kind]
  ;;  (prn 'actual-x-contains? actual expected kind)
  (cond (map? actual)
        (map-match? actual expected kind)

        :else
        (full-sequence-match? actual expected kind))
  )

(defn- add-actual [actual result]
  (if (chatty-checker-falsehood? result)
    (merge result {:actual actual})
    result))
  

;; The interface

(defn- container-checker-maker [name checker-fn]
  (tag-as-checker
   (fn [expected & kind]
      (tag-as-chatty-checker
       (named name expected
              (fn [actual]
                (add-actual actual
                            (try (checker-fn actual expected kind)
                                 (catch Error ex
                                   (noted-falsehood (.getMessage ex)))))))))))

(def contains (container-checker-maker 'contains
    (fn [actual expected kind]
      (let [ [actual expected kind] (standardized-arguments actual expected kind)]
        (cond (regex? expected)
              (try-re expected actual re-find)
                 
              :else
              (apply actual-x-contains? [actual expected kind]))))))

(def just (container-checker-maker 'just
    (fn [actual expected kind]
      (let [ [actual expected kind] (standardized-arguments actual expected kind)]
        (cond (regex? expected)
              (try-re expected actual re-matches)
            
              (same-lengths? actual expected)
              (apply actual-x-contains? [actual expected kind])

              :else
              (tag-as-chatty-falsehood
               {
                :notes [(cl-format nil
                                   "Expected ~R element~:P. There ~[were~;was~:;were~]~:* ~R."
                                   (count expected)
                                   (count actual))]}))))))

(defn- has-xfix [x-name pattern-fn take-fn]
  (fn [actual expected kind]
    (cond (set? actual)
          (tag-as-chatty-falsehood {
                                    :notes [(str "Sets don't have " x-name "es.")]})

          (map? actual)
          (tag-as-chatty-falsehood {
                                    :notes [(str "Maps don't have " x-name "es.")]})
          
          :else
          (let [ [actual expected kind] (standardized-arguments actual expected kind)]
            (cond (regex? expected)
                  (try-re (pattern-fn expected) actual re-find)
                  
                  (expected-fits? actual expected)
                  (apply actual-x-contains?
                         [(take-fn (count expected) actual) expected kind])

                  :else
                  (tag-as-chatty-falsehood
                   {
                    :notes [(str (cl-format nil
                                            "A collection with ~R element~:P cannot match a "
                                            (count actual))
                                 x-name
                                 (cl-format nil " of size ~R." (count expected)))]}))))))

(def has-prefix
     (container-checker-maker 'has-prefix
      (has-xfix "prefix" #(re-pattern (str "^" (.toString %))) take)))
(def has-suffix
     (container-checker-maker 'has-suffix
      (has-xfix "suffix" #(re-pattern (str (.toString %) "$" )) take-last)))
                            
(defn has [quantifier predicate]
  (fn [actual]
    (quantifier predicate
                (if (map? actual)
                  (vals actual)
                  actual))))

;; These are used in some internal tests. Worth publicizing?

(defn n-of [expected expected-count]
  {:midje/checker true}
  (chatty-checker [actual]
    (and (= (count actual) expected-count)
         (every? #(extended-= % expected) actual))))


(defmacro- of-functions []
  (let [names {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven",
               8 "eight", 9 "nine", 10 "ten"}
        defns (map (fn [key] `(defn ~(symbol (str (get names key) "-of")) [expected-checker#]
                                {:midje/checker true}
                                (n-of expected-checker# ~key)))
                   (keys names))]
    `(do ~@defns)))
(of-functions)
  
;; deprecated checkers

(defn map-containing [expected]
  "Accepts a map that contains all the keys and values in expected,
   perhaps along with others"
  {:midje/checker true}
  (fn [actual] 
    (= (merge actual expected) actual)))

(defn- core-array-of-maps-checker [expected actual]
  (every? (fn [one-expected-map] (some (map-containing one-expected-map) actual))
          expected))

(defn- one-level-map-flatten [list-like-thing]
  (if (map? (first list-like-thing))
    list-like-thing
    (first list-like-thing)))

(defn only-maps-containing [& maps-or-maplist]
  "Each map in the argument(s) contains some map in the expected
   result. There may be no extra maps in either the argument(s) or expected result.

   You can call this with either (only-maps-containing {..} {..}) or
   (only-maps-containing [ {..} {..} ])."
  {:midje/checker true}
  (let [expected (one-level-map-flatten maps-or-maplist)]
    (fn [actual]
      (if (= (count actual) (count expected))
        (core-array-of-maps-checker expected actual)
        false))))
  
(defn maps-containing [& maps-or-maplist]
  "Each map in the argument(s) contains contains some map in the expected
   result. There may be extra maps in the actual result.

   You can call this with either (maps-containing {..} {..}) or
   (maps-containing [ {..} {..} ])."
  {:midje/checker true}
  (let [expected (one-level-map-flatten maps-or-maplist)]
    (fn [actual]
      (if (>= (count actual) (count expected))
        (core-array-of-maps-checker expected actual)
        false))))

(defn in-any-order
  "Produces matcher that matches sequences without regard to order"
  {:midje/checker true}
  [expected]
  (fn [actual]
    (= (frequencies expected) (frequencies actual))))
