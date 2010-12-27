(ns midje.util.checkers
  (:use [clojure.set :only [difference union subset?]]
        [clojure.contrib.seq :only [rotations]]
	[clojure.contrib.combinatorics :only [permutations]]
        [midje.util.form-utils :only [regex? vector-without-element-at-index]]))

(declare chatty-checker-falsehood? captured-exception?)

;; Midje has its own peculiar idea of equality

(defn extended-fn? [x]
  (or (fn? x)
      (= (class x) clojure.lang.MultiFn)))

(defn extended-= [actual expected]
  (try
    (cond (extended-fn? expected)
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

(defn in-any-order
  "Produces matcher that matches sequences without regard to order"
  {:midje/checker true}
  [expected]
  (fn [actual]
    (= (frequencies expected) (frequencies actual))))

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
  (fn [actual] (= expected actual)))


;; Chatty checkers

(defn tag-as-chatty-falsehood [value]
  (with-meta value {:midje/chatty-checker-falsehood true}))
  
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
    `(with-meta (fn [~binding-var]
                  (let [~result-symbol (vector ~@complex-forms)]
                    (if (~function ~@substituted-arglist)
                      true
                      (let [pairs# (map vector '~complex-forms ~result-symbol)]
                        (tag-as-chatty-falsehood {:actual ~binding-var,
                                                  :intermediate-results pairs#})))))
       {:midje/chatty-checker true, :midje/checker true})))

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

; non-loops
(defn- like-a-map-entry? [elt]
  (or (and (sequential? elt) (= (count elt) 2))
      (= (class elt) clojure.lang.MapEntry)))

;; (defn singleton-to-be-wrapped? [actual expected]     ;;; To be destroyed.
;;   (cond (map? actual)
;;         (like-a-map-entry? expected)

;;         (string? actual)
;;         false

;;         (regex? actual)
;;         false

;;         (set? expected)
;;         false
        
;;         (coll? actual)
;;         (not (sequential? expected))

;;         :else
;;         true))



; Loops that look for something according to extended-equals

(defn- index-in [expected-elt actual-vector]
  (loop [index 0]
;    (println expected-elt actual-vector index)
    (cond (= index (count actual-vector))
          false

          (extended-= (actual-vector index) expected-elt)
          index

          :else
          (recur (inc index)))))

(defn- retval [actual-found actual-missed expected-found expected-missed]
  {:actual-found actual-found, :actual-missed actual-missed
   :expected-found expected-found, :expected-missed expected-missed})
  
(defmacro transfer-firsts-to [hashmap & kvs]
  (let [first-kvs (map (fn [ [key val] ] `[~key (first ~val)])
		       (partition 2 kvs))
	argmap `(hash-map ~@first-kvs)]
    `(merge-with conj ~hashmap ~argmap)))

(defn- all-expected-permutations [expected]
  (cond (and (not-any? extended-fn? expected)
	     (not-any? regex? expected))
	[expected]

	(<= (count expected) 4)
	(permutations expected)

	:else
	(rotations expected)))

(defn closer-match? [candidate best-so-far]
  (> (count (:actual-found candidate))
     (count (:actual-found best-so-far))))

(defn better-of [candidate best-so-far]
  (if (closer-match? candidate best-so-far) candidate best-so-far))


(defn tack-on-to [hashmap & kvs]
  (merge-with conj hashmap (apply (partial assoc {}) kvs)))


(defmulti seq-comparison (fn [actual expected kind]
			   (or (some #{:in-any-order} kind) :strict-order)))


(defn sequence-success? [comparison]
  (= (count (:expected-found comparison))
     (count (:expected comparison))))

(defn full-sequence-match? [actual expected kind]
  (let [comparison (seq-comparison actual expected kind)]
    (sequence-success? comparison)))

(defn in-any-order--one-permutation [actual expected kind]
  (let [starting-candidate  {:actual-found [], :expected-found [],
			     :expected-skipped-over [], :expected expected }
	gaps-ok? (some #{:gaps-ok} kind)]
    (loop [walking-actual   actual
	   walking-expected expected
	   best-so-far      starting-candidate
	   candidate        starting-candidate]

      ;; (prn "walking actual" walking-actual "walking expected" walking-expected)
      (cond (or (empty? walking-actual) (empty? walking-expected))
	    (better-of candidate best-so-far)

	    (extended-= (first walking-actual) (first walking-expected))
	    ;; window good so far, keep working on it
	    (recur (rest walking-actual)
		   (concat (:expected-skipped-over candidate) (rest walking-expected))
		   best-so-far
		   (merge 
		    (tack-on-to candidate
				:actual-found (first walking-actual)
				:expected-found (first walking-expected))
		    {:expected-skipped-over []}))

	    (not (empty? (rest walking-expected)))
	    ;; Perhaps another value in the current expected permutation will work.
	    ;; We can, after all, be in any order.
	    (recur walking-actual
		   (rest walking-expected)
		   best-so-far
		   (tack-on-to candidate
			       :expected-skipped-over (first walking-expected)))

	    (not (empty? (rest walking-actual)))
	    ;; OK, there's no match for this actual element in the whole expected.
	    (if gaps-ok?
	      ;; Since gaps are OK, we can drop the bad actual element and look for next one.
	      (recur (rest walking-actual)
		     (concat (:expected-skipped-over candidate) walking-expected)
		     (better-of candidate best-so-far)
		     (merge candidate {:expected-skipped-over []}))

	      ;; Start again with a new actual: the tail of what we started with.
	      (recur (rest (concat (:actual-found candidate) walking-actual))
		   expected
		   (better-of candidate best-so-far)
                   starting-candidate))

	    :else 
	    (better-of candidate best-so-far)))))
    
(defmethod seq-comparison :in-any-order
  [actual expected kind]

  (loop [expected-permutations (all-expected-permutations expected)
	 best-so-far {:actual-found [], :expected-found [],
		      :expected-skipped-over [], :expected expected }]
    (if (empty? expected-permutations)
      best-so-far
      (let [comparison (in-any-order--one-permutation actual
						      (first expected-permutations)
						      kind)]
	(if (sequence-success? comparison)
	  comparison
	  (recur (rest expected-permutations)
		 (better-of comparison best-so-far)))))))

(defmethod seq-comparison :strict-order
  [actual expected kind]
  (let [starting-candidate  {:actual-found [], :expected-found [], :expected expected }
	gaps-ok? (some #{:gaps-ok} kind)]
    
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

; Searches through particular types of actual results


;; (defn- actual-sequential-has-prefix? [expected actual kind]
;;   (cond (some #{:in-any-order} kind)
;;         (let [possible-prefix (take (count expected) actual)]
;;           (empty? (:expected-missed (seq-comparison possible-prefix expected kind))))
          
;;         (empty? expected)
;;         true

;;         (< (count actual) (count expected))
;;         false

;;         (extended-= (first actual) (first expected))
;;         (recur (rest expected) (rest actual) kind)

;;         :else
;;         false))


;; (defn- actual-sequential-contains? [actual expected kind]
;;   (cond (set? expected)
;; 	(recur actual (vec expected) (conj kind :in-any-order))

;; 	:else
;; 	(full-sequence-match? actual expected kind)))

;; (defn- actual-map-contains? [actual expected]
;;   (cond (map? expected)
;;         (every? (fn [key]
;;                   (and (find actual key)
;;                        (extended-= (get actual key) (get expected key))))
;;                 (keys expected))

;;         (and (sequential? expected)
;;              (every? like-a-map-entry? expected))
;;         (actual-map-contains? actual (apply hash-map (apply concat expected)))

;;         :else
;;         (throw (Error. (str "If " (pr-str actual) " is a map, " (pr-str expected) " should look like map entries.")))))

;; (defn- actual-x-contains? [actual expected kind]
;;   (cond (singleton-to-be-wrapped? actual expected)
;;         (recur actual [expected] kind)

;;         (sequential? actual)
;;         (actual-sequential-contains? actual expected kind)

;;         (set? actual)
;;         (recur (vec actual) expected [:in-any-order])

;;         (map? actual)
;;         (actual-map-contains? actual expected)

;;         (string? expected)
;;         (if (empty? kind)
;;           (.contains actual expected)
;;           (recur (vec actual) (vec expected) kind))

;;         (regex? expected)
;;         (if (empty? kind)
;;           (re-find expected actual)
;;           (throw (Error. (str "I don't know how to make sense of a "
;;                               "regular expression applied "
;; 			      kind "."))))

;;         :else
;;         false))

;; (defn- actual-x-has-prefix? [actual expected kind]
;;   (cond (singleton-to-be-wrapped? actual expected)
;;         (recur actual [expected] kind)

;;         (sequential? actual)
;;         (actual-sequential-has-prefix? actual expected kind)

;;         (string? expected)
;;         (if (empty? kind)
;;           (.startsWith actual expected)
;;           (recur (vec actual) (vec expected) kind))

;;         (regex? expected)
;;         (if (empty? kind)
;;           (re-find (re-pattern (str "^" (.toString expected))) actual)
;;           (throw (Error. (str "I don't know how to make sense of a regular expression "
;; 			      "applied " kind "."))))
;;         :else
;;         false))

  
;; (defn- actual-x-has-suffix? [actual expected kind]
;;   (cond (singleton-to-be-wrapped? actual expected)
;;         (recur actual [expected] kind)

;;         (sequential? actual)
;;         (actual-sequential-has-prefix? actual expected kind)

;;         (string? expected)
;;         (if (empty? kind)
;;           (.startsWith actual expected)
;;           (recur (vec actual) (vec expected) kind))

;;         (regex? expected)
;;         (if (empty? kind)
;;           (re-find (re-pattern (str "^" (.toString expected))) actual)
;;           (throw (Error. (str "I don't know how to make sense of a regular expression "
;; 			      "applied " kind "."))))
;;         :else
;;         false))

(defn collection-like? [thing]
  (or (coll? thing)
      (string? thing)))

(defn right-hand-singleton? [thing]
  (or (not (coll? thing)) (map? thing)))

(defn standardized-arguments [actual expected kind]
  ;; The choice to make these (throw Error) rather than be falsey is purely
  ;; one of implementation convenience.
  (cond (regex? expected)
	(cond (not (empty? kind))
	      (throw (Error. (str "I don't know how to make sense of a "
				  "regular expression applied "
				  kind "."))))

	(not (collection-like? actual))
	(throw (Error. (str "You can't compare " (pr-str actual) " (" (type actual) 
			    ") to " (pr-str expected) " (" (type expected) ").")))
	)
  

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
	      (try 
		[actual (apply hash-map (apply concat expected)) kind]
		(catch Throwable ex
		  (throw (Error. (str "If " (pr-str actual) " is a map, "
				      (pr-str expected)
				      " should look like map entries."))))))

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

(defn actual-map-contains? [actual expected kind]
  ;;  (prn "actual-map-contains" actual expected)
  (every? (fn [key]
	    (and (find actual key)
		 (extended-= (get actual key) (get expected key))))
	  (keys expected)))

(defn- actual-x-contains? [actual expected kind]
  ;;  (prn 'actual-x-contains? actual expected kind)
  (cond (map? actual)
	(actual-map-contains? actual expected kind)

	:else
	(full-sequence-match? actual expected kind))
  )

(defn same-lengths? [actual expected]
  (= (count actual) (count expected)))

(defn expected-fits? [actual expected]
  (>= (count actual) (count expected)))

(defn midje-take [n thing]
  (take n thing))

(defn midje-take-last [n thing]
  (take-last n thing))

(defn midje-re-find [re string]
  (try
    (re-find re string)
    (catch Exception ex false)))

(defn midje-re-matches [re string]
  (try
    (re-matches re string)
    (catch Exception ex false)))

;; The interface

(defmacro container-checker [checker-fn]
  `(with-meta
     (fn [expected# & kind#]
       (with-meta
	 (fn [actual#]
;	   (prn "checking" actual# expected# kind#)
	   (try (~checker-fn actual# expected# kind#)
		(catch Error ex#
;		  (prn "got error" actual# expected# kind#)
;		  (prn 		  (tag-as-chatty-falsehood {:actual actual#
;					    :notes [(.getMessage ex#)]}))

		  (tag-as-chatty-falsehood {:actual actual#
					    :notes [(.getMessage ex#)]}))))
	 {:midje/chatty-checker true, :midje/checker true}))
     {:midje/checker true}))

(def contains (container-checker
    (fn [actual expected kind]
      (let [ [actual expected kind] (standardized-arguments actual expected kind)]
	(cond (regex? expected)
	      (midje-re-find expected actual)
		 
	      :else
	      (apply actual-x-contains? [actual expected kind]))))))

(def just (container-checker
    (fn [actual expected kind]
      (let [ [actual expected kind] (standardized-arguments actual expected kind)]
	(cond (regex? expected)
	      (midje-re-matches expected actual)
	    
	      (same-lengths? actual expected)
	      (apply actual-x-contains? [actual expected kind])

	      :else
	      false)))))

(defn has-xfix [x-name pattern-fn take-fn]
  (fn [actual expected kind]
    (cond (set? actual)
	  (tag-as-chatty-falsehood {:actual actual
				    :notes [(str "Sets don't have " x-name "es.")]})

	  :else
	  (let [ [actual expected kind] (standardized-arguments actual expected kind)]
	    (cond (regex? expected)
		  (midje-re-find (pattern-fn expected) actual)
		  
		  (expected-fits? actual expected)
		  (apply actual-x-contains?
			 [(take-fn (count expected) actual) expected kind])

		  :else
		  false)))))

(def has-prefix
     (container-checker
      (has-xfix "prefix" #(re-pattern (str "^" (.toString %))) midje-take)))
(def has-suffix
     (container-checker
      (has-xfix "suffix" #(re-pattern (str (.toString %) "$" )) midje-take-last)))
			    
;; (def has-prefix (container-checker
;;    (fn [actual expected kind]
;;      (cond (set? actual)
;; 	   (tag-as-chatty-falsehood {:actual actual
;; 				     :notes ["Sets don't have prefixes."]})

;; 	   :else
;; 	   (let [ [actual expected kind] (standardized-arguments actual expected kind)]
;; 	     (cond (regex? expected)
;; 		   (midje-re-find ) actual)
	    
;; 		   (expected-fits? actual expected)
;; 		   (apply actual-x-contains?
;; 			  [(midje-take (count expected) actual) expected kind])

;; 		   :else
;; 		   false))))))

;; (def has-suffix (container-checker
;;     (fn [[actual expected kind]]
;;       (cond (regex? expected)
;; 	    (midje-re-find ) actual)
      
;; 	    (expected-fits? actual expected)
;; 	    (apply actual-x-contains?
;; 		   [(midje-take-last (count expected) actual) expected kind])

;; 	    :else
;; 	    false))))

(defn has [quantifier predicate]
  (fn [actual]
    (quantifier predicate
		(if (map? actual)
		  (vals actual)
		  actual))))

(defn n-of [expected expected-count]
  {:midje/checker true}
  (chatty-checker [actual]
    (and (= (count actual) expected-count)
         (every? #(extended-= % expected) actual))))


(defmacro of-functions []
  (let [names {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven",
               8 "eight", 9 "nine", 10 "ten"}
        defns (map (fn [key] `(defn ~(symbol (str (get names key) "-of")) [expected-checker#]
                                {:midje/checker true}
                                (n-of expected-checker# ~key)))
                   (keys names))]
    `(do ~@defns)))
(of-functions)
  
;; Deprecated checkers

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
