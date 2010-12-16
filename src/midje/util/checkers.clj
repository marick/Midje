(ns midje.util.checkers
  (:use [clojure.set :only [subset?]]))



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
	    (= message (.getMessage (wrapped-throwable captured-exception-key))))))
)


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

(defn function-aware-= [actual expected]
  (if (fn? expected) 
    (let [function-result (expected actual)]
      (if (chatty-checker-falsehood? function-result) false function-result))
    (= actual expected)))

(defn- prefix? [smaller bigger]
  (cond (empty? smaller)
	true

	(function-aware-= (first bigger) (first smaller))
	(prefix? (rest smaller) (rest bigger))

	:else
	false))
			  

(defn- sequential-contains [bigger smaller]
;  (println 'seq=-contains bigger smaller)
  (cond (< (count bigger) (count smaller))
	false

	(prefix? smaller bigger)
	true
	
	:else
	(recur (rest bigger) smaller)))

(defn- contains-guts [bigger smaller]
  (cond (map? smaller)
	(every? (fn [key]
		  (and (find bigger key)
		       (function-aware-= (get bigger key) (get smaller key))))
		(keys smaller))

	(sequential? smaller)
	(sequential-contains bigger smaller)

	(and (string? smaller) (string? bigger))
	(.contains bigger smaller)

	(= (class smaller) java.util.regex.Pattern)
	(re-find smaller bigger)

	(set? smaller)
	false

	:else
	(some #(function-aware-= % smaller) bigger)))
	


;; Work in progress
(defn contains [expected]
  {:midje/checker true}
  (fn [actual] (contains-guts actual expected)))
    

(defn n-of [expected expected-count]
  {:midje/checker true}
  (chatty-checker [actual]
    (and (= (count actual) expected-count)
	 (if (fn? expected)
	   (every? expected actual)
	   (every? (partial = expected) actual)))))


(defmacro of-functions []
  (let [names {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven",
	       8 "eight", 9 "nine", 10 "ten"}
	defns (map (fn [key] `(defn ~(symbol (str (get names key) "-of")) [expected-checker#]
				{:midje/checker true}
				(n-of expected-checker# ~key)))
		   (keys names))]
    `(do ~@defns)))
(of-functions)
  
