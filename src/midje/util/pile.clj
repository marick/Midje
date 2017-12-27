(ns midje.util.pile
  "Functions that are somewhat general purpose."
  (:require [midje.util.ordered-map :refer [ordered-map]]
            [such.maps :as map]))

;;; Named things

(defn named-function? [x]
  (:name (meta x)))

(defn object-name [obj]
  (:name (meta obj)))

(defn function-name [funobj]
  (object-name funobj))

(defn name-object [object name]
  (vary-meta object assoc :name name))

;;; Maps

(defn sort-map [m]
  (into (sorted-map) m))

;;; Copied from utilize to remove dependencies.
;;; https://github.com/AlexBaranosky/Utilize
(defn ordered-zipmap [keys vals]
  "Like zipmap, but guarantees order of the entries"
  (loop [m (ordered-map)
         ks (seq keys)
         vs (seq vals)]
    (if (and ks vs)
      (recur (assoc m (first ks) (first vs))
             (next ks)
             (next vs))
      m)))

;;; Sequences

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [coll]
  (for [i (range 0 (count coll))]
    (lazy-cat (drop i coll) (take i coll))))

(defn map-first
  "Like map, but applies f to only the first element of the seq"
  [f x]
  (cons (f (first x)) (rest x)))

(defn apply-pairwise
  "(apply-pairwise [inc dec] [1 1] [2 2]) => [ [2 0] [3 1] ]
   Note that the functions must take only a single argument."
  [functions & arglists]
  (map (partial map
                (fn [f arg] (f arg))
                functions)
       arglists))

(defn pop-if
  "Extracts optional arg (that we assume is present if the pred is true) from head of args"
  [pred args]
  (if (pred (first args))
    [(first args) (rest args)]
    [nil args]))

(def pop-docstring
  ;; "Extracts optional map from head of args"
  (partial pop-if string?))

(def pop-opts-map
  ;; "Extracts optional docstring from head of args"
  (partial pop-if map?))


;;; Definition helpers

(defmacro macro-for
  "Macroexpands the body once for each of the elements in the
   right-side argument of the bindings, which should be a seq"
  [bindings body]
  `(let [macros# (for ~bindings
                     ~body)]
    `(do ~@macros#)))

(defmacro def-many-methods
  "Create multiple multimethods with different dispatch values
   but the same implementation"
  [name dispatch-vals args & body]
  (macro-for [dval dispatch-vals]
    `(defmethod ~name ~dval ~args
       ~@body)))


;;; Randomness

(defn stringlike-matches? [stringlike given]
  (cond (not (string? given))
        false

        (string? stringlike)
        (.contains ^String given stringlike)

        :else
        (boolean (re-find stringlike given))))

