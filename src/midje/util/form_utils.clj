;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Utility functions dealing with checking or tranforming forms."}
  midje.util.form-utils
  (:use [midje.util.treelike :only [tree-variant]]
        [clojure.set :only [difference]]
        [utilize.seq :only (first-truthy-fn)])
  (:require [clojure.zip :as zip]))

(defn unique-argument-name []
  (gensym 'symbol-for-destructured-arg))

(defn single-arg-into-form-and-name [arg-form]
  (cond (vector? arg-form)
        (if (= :as (second (reverse arg-form)))  ; use existing as
          [ arg-form (last arg-form)]
          (let [as-symbol (unique-argument-name)]
            [ (-> arg-form (conj :as) (conj as-symbol))
              as-symbol]))
    
        (map? arg-form)
        (if (contains? arg-form :as)
          [ arg-form (:as arg-form)]
          (let [as-symbol (unique-argument-name)]
            [ (assoc arg-form :as as-symbol)
              as-symbol]))        
       
        :else 
        [arg-form arg-form]))


(defn regex? [x]
  (= (class x) java.util.regex.Pattern))

(defn classic-map? [x]
  (.isInstance clojure.lang.APersistentMap x))
  
(defn record? [x]
  (and (map? x) (not (classic-map? x))))
  
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


(defn reader-list-form?
  "True if the form is a parenthesized list of the sort the reader can return."
  [form]
  (or (list? form) (= (type form) clojure.lang.Cons)))

(defn quoted-list-form?
  "True if the form is a quoted list such as the reader might return"
  [form]
  (and (reader-list-form? form)
       (quoted? form)))

(defn preserve-type
  "If the original form was a vector, make the transformed form a vector too."
  [original-form transformed-form]
  (if (vector? original-form)
    (vec transformed-form)
    transformed-form))

(defn separate-by
  "Like clojure.core/separate, but not lazy, returns nil for empty list."
  ;; TODO: One of those two differences is the difference between
  ;; a blowup in PersistentArrayMap and no blowup. Should investigate.
  [predicate forms]
  (let [ensure-truthful (comp not not predicate)]
    (let [group (group-by ensure-truthful forms)]
      [ (group true) (group false) ])))

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

(defn hash-map-duplicates-ok
  "Like hash-map, except duplicate keys are OK. Last one takes precedence." 
  [& keys-and-vals]
  (if (empty? keys-and-vals)
    {}
    (apply assoc {} keys-and-vals)))     

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

(defn rotations
  "Returns a lazy seq of all rotations of a seq"
  [coll]
  (map-indexed
    (fn [idx _]
      (lazy-cat (drop idx coll) (take idx coll)))
    coll))

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

(defmacro macro-for 
  "Macroexpands the body once for each of the elements in the 
   right-side argument of the bindings, which should be a seq"
  [bindings body] 
  `(let [macros# (for ~bindings
                     ~body)]
    `(do ~@macros#)))

(defn map-first
  "Like map, but applies f to only the first element of the seq"
  [f x]
  (cons (f (first x)) (rest x)))

(defn named? [x]
  (instance? clojure.lang.Named x))

(defn sort-map [m]
  (into (sorted-map) m))

(defmacro def-many-methods 
  "Create multiple multimethods with different dispatch values 
   but the same implementation"
  [name dispatch-vals args & body] 
  (macro-for [dval dispatch-vals]
    `(defmethod ~name ~dval ~args
       ~@body)))

;;;; stolen from `useful`

(defn var-name
  "Get the namespace-qualified name of a var."
  [v]
  (apply symbol (map str ((juxt (comp ns-name :ns)
                            :name)
                           (meta v)))))

(defn alias-var
  "Create a var with the supplied name in the current namespace, having the same
metadata and root-binding as the supplied var."
  [name ^clojure.lang.Var var]
  (apply intern *ns* (with-meta name (merge {:dont-test (str "Alias of " (var-name var))}
                                       (meta var)
                                       (meta name)))
    (when (.hasRoot var) [@var])))

(defmacro defalias
  "Defines an alias for a var: a new var with the same root binding (if
any) and similar metadata. The metadata of the alias is its initial
metadata (as provided by def) merged into the metadata of the original."
  [dst src]
  `(alias-var (quote ~dst) (var ~src)))

;;;;

(defmacro to-thunks
  "Takes a seq of unevaluated exprs. Returns a seq of no argument fns, 
  that call each of the exprs in turn"
  [exprs]
  (vec (for [x exprs]
         `(fn [] ~x))))

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
