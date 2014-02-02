(ns ^{:doc "Functions I wouldn't mind to see in clojure.core"}
  midje.clojure.core
  (:require clojure.pprint
            clojure.set
            utilize.seq
            swiss.arrows
            midje.clojure.backwards-compatibility))

;; Note: some of this code is taken from https://github.com/flatland/useful/blob/develop/src/flatland/useful/ns.clj
;; Those functions should be immigrated once the new useful namespace is pushed to clojars.

;;; Types and pseudo types

(defn regex? [x]
  (= (class x) java.util.regex.Pattern))

(defn stringlike?
  "String or regex"
  [x]
  (or (string? x)
      (regex? x)))

(defn classic-map? [x]
  (.isInstance clojure.lang.APersistentMap x))
  
(defn record? [x]
  (and (map? x) (not (classic-map? x))))

(defn extended-fn? [x]
  (or (fn? x)
    (= (class x) clojure.lang.MultiFn)))
  
(defn named? [x]
  (instance? clojure.lang.Named x))

(defn listlike? [x]
  (or (list? x)
      (seq? x)))


;;; Annoyances

(defn strictly [loose-predicate]
  (comp boolean loose-predicate))

(def any? (strictly some))

(def not-empty? (strictly seq))


;;; Vars

(defn var-root [var]
  (alter-var-root var identity))

(defn var-name ;; from `useful`
  "Get the namespace-qualified name of a var."
  [v]
  (apply symbol (map str ((juxt (comp ns-name :ns)
                            :name)
                          (meta v)))))


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

(letfn [(move-var [var sym]
          (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
            (if (.hasRoot var)
              (intern *ns* sym (var-root var))
              (intern *ns* sym))))]

  (defn immigrate
    "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, root
  binding, and metadata as the original except that their :ns metadata
  value is this namespace."
    [& ns-names]
    (doseq [ns ns-names]
      (require ns)
      (doseq [[sym ^clojure.lang.Var var] (ns-publics ns)]
        (move-var var sym))))
  
  (defn immigrate-from
    "Like `immigrate`, except wth a list of named symbols."
    [ns symbols]
    (doseq [sym symbols]
      (move-var (ns-resolve ns sym) sym))))

;;; Maps

(defn hash-map-duplicates-ok
  "Like hash-map, except duplicate keys are OK. Last one takes precedence." 
  [& keys-and-vals]
  (if (empty? keys-and-vals)
    {}
    (apply assoc {} keys-and-vals)))     

(defn invert
  "Produce a map with values as keys.
   Values are assumed unique."
  [map]
  (reduce (fn [so-far [key val]]
            (assoc so-far val key))
          {}
          map))

(defn dissoc-keypath
  "Like `dissoc`, but takes a sequence of keys.
   There must be at least two keys."
  [map keys]
  (let [[path-to-end-key end-key] [(butlast keys) (last keys)]
        ending-container (get-in map path-to-end-key)
        without-key (dissoc ending-container end-key)]
    (assoc-in map path-to-end-key without-key)))


(defn transform-in
  ([map keyseq f default]
     (assoc-in map keyseq
               (f (get-in map keyseq default))))
  ([map keyseq f]
     (transform-in map keyseq f nil)))

(defn transform
  ([map key f] (transform-in map [key] f))
  ([map key f default] (transform-in map [key] f default)))
  



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

(defn vertical-slices
  "Given N sequences, return one sequence whose first element
   is a sequence of all the first elements, etc."
  [& sequences]
  (apply (partial map (fn [& args] args)) sequences))

(immigrate-from 'utilize.seq '[separate find-first only])

;;; Sets

(immigrate-from 'clojure.set '[union difference subset? intersection])

;;; Control flow

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

(immigrate-from 'swiss.arrows '[-<>])

;;; Printing

(immigrate-from 'clojure.pprint '[pprint cl-format])

;;; Compatibility

(immigrate 'midje.clojure.backwards-compatibility)
