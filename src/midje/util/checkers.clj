(ns midje.util.checkers
  (:use [clojure.set :only [difference subset?]]
	[midje.util.form-utils :only [regex? vector-without-element-at-index]]))

(declare chatty-checker-falsehood? captured-exception?)

;; Midje has its own peculiar idea of equality

(defn extended-= [actual expected]
  (cond (fn? expected)
	(try
	  (let [function-result (expected actual)]
	    (if (chatty-checker-falsehood? function-result) false function-result))
	  (catch Exception ex false))
	
	(regex? expected)
	(if (regex? actual)
	  (= (.toString actual) (.toString expected))
	  (re-find expected actual))

	:else
	(= actual expected)))


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

(defn singleton-to-be-wrapped? [actual expected]
  (cond (map? actual)
	(like-a-map-entry? expected)

	(string? actual)
	false

	(regex? actual)
	false

	(set? expected)
	false
	
	(coll? actual)
	(not (sequential? expected))

	:else
	true))

; Loops that look for something according to extended-equals

(defn- index-of-actual [expected-elt actual-vector]
  (loop [index 0]
;    (println expected-elt actual-vector index)
    (cond (= index (count actual-vector))
	  false

	  (extended-= (actual-vector index) expected-elt)
	  index

	  :else
	  (recur (inc index)))))

(defn- unordered-seq-comparison [actual expected]
  (loop [actual (vec actual)
	 expected (list* expected)
	 actual-found []
	 expected-found []
	 expected-missed []]
;    (prn expected actual-found actual expected-found expected-missed)
    (if (empty? expected)
      {:actual-found actual-found
       :actual-missed actual,
       :expected-found expected-found,
       :expected-missed, expected-missed}
      (if-let [index (index-of-actual (first expected) actual)]
	(recur (vector-without-element-at-index index actual)
	       (rest expected)
	       (conj actual-found (actual index))
	       (conj expected-found (first expected))
	       expected-missed)
	(recur actual
	       (rest expected)
	       actual-found
	       expected-found
	       (conj expected-missed (first expected)))))))

(defn- actual-sequential-has-prefix? [actual expected]
  (cond (empty? expected)
	true

        (< (count actual) (count expected))
	false

	(extended-= (first actual) (first expected))
	(recur (rest actual) (rest expected))

	:else
	false))

; Searches through particular types of actual results

(defn- actual-sequential-contains? [actual expected order]
  (cond (= order :in-any-order)
	(empty? (:expected-missed (unordered-seq-comparison actual expected)))

	(set? expected)
	(recur actual (vec expected) :in-any-order)

        (< (count actual) (count expected))
	false

	(actual-sequential-has-prefix? actual expected)
	true
	
	:else
	(recur (rest actual) expected order)))

(defn- actual-map-contains? [actual expected]
  (cond (map? expected)
	(every? (fn [key]
	 	  (and (find actual key)
	 	       (extended-= (get actual key) (get expected key))))
	 	(keys expected))

	(and (sequential? expected)
	     (every? like-a-map-entry? expected))
	(actual-map-contains? actual (apply hash-map (apply concat expected)))

	:else
	(throw (Error. (str "If " (pr-str actual) " is a map, " (pr-str expected) " should look like map entries.")))))

(defn- actual-x-contains? [actual expected order]
  (cond (singleton-to-be-wrapped? actual expected)
	(recur actual [expected] order)

	(sequential? actual)
	(actual-sequential-contains? actual expected order)

	(set? actual)
	(recur (vec actual) expected :in-any-order)

        (map? actual)
	(actual-map-contains? actual expected)

	(string? expected)
	(if (= order :in-any-order)
	  (recur (vec actual) (vec expected) :in-any-order)
	  (.contains actual expected))

	(regex? expected)
	(if (= order :in-any-order)
	  (throw (Error. "I don't know how to make sense of a regular expression applied :in-any-order."))
	  (re-find expected actual))

	:else
	false))

(defn- x-has-prefix? [actual expected]
  (cond (singleton-to-be-wrapped? actual expected)
	(recur actual [expected])

	(sequential? actual)
	(actual-sequential-has-prefix? actual expected)

	(string? expected)
	(.startsWith actual expected)

	(regex? expected)
	(re-find (re-pattern (str "^" (.toString expected)))
		 actual)
	:else
	false))

;; The interface

(defn contains 
  {:midje/checker true}
  ([expected]
     (fn [actual] (actual-x-contains? actual expected :ordered)))
  ([expected order]
     (fn [actual] (actual-x-contains? actual expected order))))
  

(defn has-prefix [expected]
  {:midje/checker true}
  (fn [actual] (x-has-prefix? actual expected)))

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
