(ns ^{:doc "Deprecated checkers."}
  midje.checkers.deprecated
  (:use [midje.checkers.defining :only [defchecker]]
        [midje.util.deprecation :only [deprecate]]
      	[midje.checkers.collection :only [just contains]]))

;; Note: checkers need to be exported in ../checkers.clj

(defchecker map-containing 
  "DEPRECATED: use 'contains' checker instead.
   Accepts a map that contains all the keys and values in expected,
   perhaps along with others"
  {:deprecated "0.9.0"
   :arglists '([expected])}
  [expected]
  (deprecate "`map-containing will be removed in 1.6. Use `contains` instead.")
  (contains expected))

(letfn [(one-level-map-flatten [list-like-thing]
          (if (map? (first list-like-thing))
            list-like-thing
            (first list-like-thing)))]

  (defchecker only-maps-containing 
    "DEPRECATED: use (just [ (contains {..}) ]) instead.
     Each map in the argument(s) contains some map in the expected
     result. There may be no extra maps in either the argument(s) or expected result.
  
     You can call this with either (only-maps-containing {..} {..}) or
     (only-maps-containing [ {..} {..} ])."
    {:deprecated "0.9.0"
     :arglists '([maps-or-maplist])}
    [& maps-or-maplist]
    (deprecate "`only-maps-containing will be removed in 1.6. Use `(just [ (contains {..}) ])` instead.")
    (let [expected (one-level-map-flatten maps-or-maplist)
          subfunctions (map contains expected)]
      (just subfunctions :in-any-order)))
  
  (defchecker maps-containing 
    "DEPRECATED: use (contains [ (contains {..}) ] instead.
     Each map in the argument(s) contains contains some map in the expected
     result. There may be extra maps in the actual result.
  
     You can call this with either (maps-containing {..} {..}) or
     (maps-containing [ {..} {..} ])."
    {:deprecated "0.9.0"
     :arglists '([maps-or-maplist])}
    [& maps-or-maplist]
    (deprecate "`maps-containing will be removed in 1.6. Use `(contains [ (contains {..}) ])` instead.")
    (let [expected (one-level-map-flatten maps-or-maplist)
          subfunctions (map contains expected)]
      (contains subfunctions :in-any-order :gaps-ok))))

(defchecker in-any-order
  "DEPRECATED: use (just x :in-any-order) instead. 
   Produces matcher that matches sequences without regard to order."
  {:deprecated "0.9.0"
   :arglists '([expected])}
  [expected]
  (deprecate "`in-any-order will be removed in 1.6. Use `(just x :in-any-order)` instead.")
  (just expected :in-any-order))
