(ns ^{:doc "Utility functions dealing with checking or tranforming forms."}
  midje.util.form-utils
  (:use midje.clojure.core
   [midje.util.treelike :only [tree-variant]]
   [utilize.seq :only (first-truthy-fn)])
  (:require [clojure.zip :as zip]))


;;;; Import pprint and cl-format


(defn stringlike-matches? [stringlike given]
  (cond (not (string? given))
        false

        (string? stringlike)
        (.contains given stringlike)

        :else
        (boolean (re-find stringlike given))))

(defn symbol-named?
  "Is the thing a symbol with the name given by the string?"
  [x string]
  (and (symbol? x)
       (= (name x) string)))

(defn first-named?
  "Is the form's first element a symbol whose name is the desired string?"
  [form desired]
  (and (sequential? form)
       (symbol-named? (first form) desired)))

(defmulti quoted? tree-variant)
(defmethod quoted? :zipper [loc]
  (quoted? (zip/node loc)))
(defmethod quoted? :form [form]
  (first-named? form "quote"))

(def dequote #(if (quoted? %) (second %) %))


(defn reader-list-form?
  "True if the form is a parenthesized list of the sort the reader can return."
  [form]
  (or (list? form) (= (type form) clojure.lang.Cons)))

(defn quoted-list-form?
  "True if the form is a quoted list such as the reader might return"
  [form]
  (and (reader-list-form? form)
       (quoted? form)))

;;; Some higher-order predicate helpers

(defn any-pred-from
  "Returns a function that returns strictly true iff any
   of the predicates is truthy of the function's single argument.
   ( (any-of? even? odd?) 3) => true
   Stops checking after first success."
  [preds]
  (if (empty? preds)
    (constantly true)
    (fn [arg]
      (loop [[candidate & remainder :as preds] preds]
        (cond (empty? preds)  false
              (candidate arg) true
              :else           (recur remainder))))))
  

;;; Etc.

(defn preserve-type
  "If the original form was a vector, make the transformed form a vector too."
  [original-form transformed-form]
  (if (vector? original-form)
    (vec transformed-form)
    transformed-form))

(defn reader-line-number 
  "Find what line number the reader put on the given form or on
   one of its elements. If no line numbers, a warning string."
  [form]
  (or (:line (meta form))
      (some (comp :line meta) form)
      "0 (no line info)"))

(defn vector-without-element-at-index [index v]
  (vec (concat (subvec v 0 index) (subvec v (inc index)))))

(defn pairs
  "Return [ (first first-seq) (first second-seq)] ..."
  [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn tack-on-to
  "Conj new values onto appropriate keys of a map" 
  [hashmap & kvs]
  (merge-with conj hashmap (apply hash-map-duplicates-ok kvs)))

(defn apply-pairwise
  "(apply-pairwise [inc dec] [1 1] [2 2]) => [ [2 0] [3 1] ]
   Note that the functions must take only a single argument." 
  [functions & arglists]
  (map (partial map 
  		(fn [f arg] (f arg)) 
  		functions) 
  	arglists))

(defn map-difference [bigger smaller]
  (select-keys bigger (difference (set (keys bigger)) (set (keys smaller)))))

(defn translate-zipper
  "Traverses the zipper - for the first predicate that evaluates to truthy for matching a
  node, calls the corresponding translate function on that node. Then, continues traversing."
  [form & preds+translate-fns]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (if-let [truthy-fn (first-truthy-fn (take-nth 2 preds+translate-fns) loc)]
        (recur (zip/next ((get (apply hash-map preds+translate-fns) truthy-fn) loc)))
        (recur (zip/next loc))))))

(defmacro pred-cond 
  "Checks each predicate against the item, returning the corresponding 
   result if it finds a match, otherwise returning nil.
   Assumes item to be a value, as it will get evaluated multiple times."
  [item pred result & preds+results]
  (cond (= pred :else ) result
        (not (seq preds+results)) `(if (~pred ~item) ~result nil) ;; last condition, but no :else in the form
        :else `(if (~pred ~item)
                 ~result
                 (pred-cond ~item ~@preds+results))))

(defn single-destructuring-arg->form+name [arg-form]
  (let [as-symbol          (gensym 'symbol-for-destructured-arg)
        snd-to-last-is-as? #(= :as (second (reverse %)))
        has-key-as?        #(contains? % :as)]
    (pred-cond arg-form
      (every-pred-m vector? snd-to-last-is-as?) [arg-form (last arg-form)]
      vector?                                   [(-> arg-form (conj :as) (conj as-symbol)) as-symbol]
      (every-pred-m map? has-key-as?)           [arg-form (:as arg-form)]
      map?                                      [(assoc arg-form :as as-symbol) as-symbol]
      :else                                     [arg-form arg-form] )))

(defmacro macro-for 
  "Macroexpands the body once for each of the elements in the 
   right-side argument of the bindings, which should be a seq"
  [bindings body] 
  `(let [macros# (for ~bindings
                     ~body)]
    `(do ~@macros#)))

(defn sort-map [m]
  (into (sorted-map) m))

(defmacro def-many-methods 
  "Create multiple multimethods with different dispatch values 
   but the same implementation"
  [name dispatch-vals args & body] 
  (macro-for [dval dispatch-vals]
    `(defmethod ~name ~dval ~args
       ~@body)))



;;;;

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



(defn midje-position-string [[filename line-num]]
  (format "(%s:%s)" filename line-num))

