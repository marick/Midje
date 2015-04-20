(ns ^{:doc "Functions that are somewhat general purpose, but seem too obscure to be in marick.clojure.core.
           A cue to wiser placement decisions someday."}
  midje.util.pile
  (:use marick.clojure.core)
  (:use [ordered.map :only [ordered-map]])
  (:import org.apache.commons.codec.digest.DigestUtils))

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

(defn tack-on-to
  "Conj new values onto appropriate keys of a map" 
  [hashmap & kvs]
  (merge-with conj hashmap (apply hash-map-duplicates-ok kvs)))

(defn map-difference [bigger smaller]
  (select-keys bigger (difference (set (keys bigger)) (set (keys smaller)))))

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



;;; Namespaces

(defn alias-var  ;; from `useful`
  "Create a var with the supplied name in the current namespace, having the same
metadata and root-binding as the supplied var."
  [name ^clojure.lang.Var var]
  (apply intern *ns*
         (with-meta name (merge {:dont-test (str "Alias of " (var-name var))}
                                (meta var)
                                (meta name)))
         (when (.hasRoot var) [@var])))


(defmacro defalias  ;; from `useful`
  "Defines an alias for a var: a new var with the same root binding (if
any) and similar metadata. The metadata of the alias is its initial
metadata (as provided by def) merged into the metadata of the original."
  [dst src]
  `(alias-var (quote ~dst) (var ~src)))

;;; Higher-order predicate helpers

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

;;; Hashing

(defn form-guid [form]
  (DigestUtils/shaHex (pr-str form)))

;;; Randomness

(defn stringlike-matches? [stringlike given]
  (cond (not (string? given))
        false

        (string? stringlike)
        (.contains given stringlike)

        :else
        (boolean (re-find stringlike given))))

