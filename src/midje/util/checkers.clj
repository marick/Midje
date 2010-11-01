(ns midje.util.checkers)

(if (re-find #"1.1" (clojure-version))
  (use '[clojure.contrib.seq-utils :only [frequencies]]))


(defn truthy 
  "Returns precisely true if actual is not nil and not false."
  {:midje/checker true}
  [actual] 
  (not (not actual)))

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
  true)
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
  "Each element in the actual result matches some map in the expected
   result, where 'match' means #'contains-map. There may be no extra
   maps in either the actual or expected result.

   You can call this with either (only-maps-containing {..} {..}) or
   (only-maps-containing [ {..} {..} ])."
  {:midje/checker true}
  (let [expected (one-level-map-flatten maps-or-maplist)]
    (fn [actual]
      (if (= (count actual) (count expected))
	(core-array-of-maps-checker expected actual)
	false))))
  
(defn maps-containing [& maps-or-maplist]
  "Each element in the actual result matches some map in the expected
   result, where 'match' means #'contains-map. There may be extra
   maps in the actual result.

   You can call this with either (maps-containing {..} {..}) or
   (maps-containing [ {..} {..} ])."
  {:midje/checker true}
  (let [expected (one-level-map-flatten maps-or-maplist)]
    (fn [actual]
      (if (>= (count actual) (count expected))
	(core-array-of-maps-checker expected actual)
	false))))
  
  
  



(def #^{:private true} captured-exception-key "this Throwable was captured by midje:")
(defn captured-exception [e] {captured-exception-key e})

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
     

(defn tag-as-chatty-falsehood [value]
  (with-meta value {:midje/chatty-checker-falsehood true}))
  
(defn chatty-checker-falsehood? [value]
  (:midje/chatty-checker-falsehood (meta value)))

(defn chatty-checker? [fn]
  (:midje/chatty-checker (meta fn)))

(defn chatty-checker*
  "Create a function that returns either true or a detailed description of a failure."
  [actual-processor final-comparison]
  (with-meta (fn [expected]
	       (with-meta (fn [actual]
			    (let [processed-actual (actual-processor actual)]
			      (if (final-comparison processed-actual expected)
				true
				(tag-as-chatty-falsehood {:actual actual,
							  :actual-processor actual-processor
							  :processed-actual processed-actual}))))
		 {:midje/chatty-checker true}))
    {:midje/checker true}))

(defmacro chatty-checker
  [ [ final-comparison [actual-processor actual-placeholder] expected-placeholder] ]
  "Create a function that returns either true or a detailed description of a failure."
  `(chatty-checker* (var ~actual-processor) (var ~final-comparison)))
