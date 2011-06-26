(ns midje.checkers.deprecated
  (:use [clojure.set :only [difference union subset?]]
        [clojure.contrib.seq :only [rotations]]
        [clojure.contrib.def :only [defmacro- defvar-]]
        [clojure.contrib.pprint :only [cl-format]]
        [clojure.contrib.combinatorics :only [permutations]]
        [midje.util.form-utils :only [regex? vector-without-element-at-index
                                      tack-on-to]]
	[midje.checkers.collection :only [just contains]]
	[midje.checkers defining util]))

;; Note: checkers need to be exported in ../checkers.clj

(defchecker map-containing 
  "Accepts a map that contains all the keys and values in expected,
   perhaps along with others"
  [expected]
  (contains expected))

(defn- one-level-map-flatten [list-like-thing]
  (if (map? (first list-like-thing))
    list-like-thing
    (first list-like-thing)))

(defchecker only-maps-containing 
  "Each map in the argument(s) contains some map in the expected
   result. There may be no extra maps in either the argument(s) or expected result.

   You can call this with either (only-maps-containing {..} {..}) or
   (only-maps-containing [ {..} {..} ])."
  [& maps-or-maplist]
  (let [expected (one-level-map-flatten maps-or-maplist)
        subfunctions (map contains expected)]
    (just subfunctions :in-any-order)))
  
(defchecker maps-containing 
  "Each map in the argument(s) contains contains some map in the expected
   result. There may be extra maps in the actual result.

   You can call this with either (maps-containing {..} {..}) or
   (maps-containing [ {..} {..} ])."
  [& maps-or-maplist]
  (let [expected (one-level-map-flatten maps-or-maplist)
        subfunctions (map contains expected)]
    (contains subfunctions :in-any-order :gaps-ok)))

(defchecker in-any-order
  "Produces matcher that matches sequences without regard to order.
   Prefer (just x :in-any-order)."
  [expected]
  (just expected :in-any-order))
