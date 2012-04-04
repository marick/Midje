(ns ^{:doc "A notation that avoids confusion between what’s essential 
            about data and what’s accidental. A stand in for constant data."}
  midje.ideas.metaconstants
  (:use [midje.util.form-utils :only [translate-zipper]]
        [midje.util.zip :only [skip-down-then-rightmost-leaf]]
        [midje.util.thread-safe-var-nesting :only [unbound-marker]]
        [midje.error-handling.exceptions :only [user-error]]
        [utilize.seq :only (find-first)]
        [clojure.core.incubator :only [-?>>]])
  (:require [clojure.zip :as zip]
            [midje.util.ecosystem :as ecosystem]))

(defn- normalized-metaconstant* [dot-or-dash mc-symbol]
  (let [re (re-pattern (str "^" dot-or-dash "+(.+?)" dot-or-dash "+$"))
        three-dots-or-dashes (apply str (repeat 3 dot-or-dash))]
    (-?>> mc-symbol 
          name 
          (re-matches re) 
          second 
          (format (str three-dots-or-dashes "%s" three-dots-or-dashes)))))

(defn- normalized-metaconstant
  "Turns '..m. to \"...m...\", '--m- to \"---m---\", or to 
   nil if mc-symbol isn't a valid metaconstant symbol"
  [mc-symbol]
  (->> mc-symbol 
       ((juxt (partial normalized-metaconstant* "\\.") (partial normalized-metaconstant* "-"))) 
       (find-first identity)))

(defn metaconstant-symbol? [x]
  (and (symbol? x)
    (-> x normalized-metaconstant not not)))

(deftype Metaconstant [name ^clojure.lang.Associative storage]
  Object
  (toString [this]
            (str (.name this)))
  (equals [^Metaconstant this that]
         (if (instance? (class this) that)
           (= (normalized-metaconstant (.name this)) (normalized-metaconstant (.name ^Metaconstant that)))
           (= (normalized-metaconstant (.name this)) (normalized-metaconstant that))))

  clojure.lang.ILookup
  (valAt [this key]
         (get storage key))
  (valAt [this key default]
         (get storage key default))

  clojure.lang.IFn
  (invoke [this key]
          (get storage key))
  (invoke [this key default]
          (get storage key default))

  clojure.lang.Associative
  (containsKey [this key]
               (.containsKey storage key))
  (entryAt [this key]
           (find storage key))
  (assoc [this key val]
    ;; (Metaconstant. (.name this) (assoc storage key val))) 
    (throw (user-error 
            (str "Metaconstants (" (.name this) ") can't have values assoc'd onto them.")
            "If you have a compelling need for that, please create an issue:"
            ecosystem/issues-url)))

  ;; Next two interfaces are extended by Associative.
  clojure.lang.Seqable
  (seq [this] (seq storage))  ; I'm unhappy to define this, but it's needed by count/empty.

  clojure.lang.IPersistentCollection
  (count [this]
         (count storage))
  (cons [this o]
        ;; (Metaconstant. (.name this) (cons storage o)))
        (throw (user-error
                (str "Metaconstants (" (.name this) ") can't have values added onto them.")
                "If you have a compelling need for that, please create an issue:"
                ecosystem/issues-url)))
  (empty [this]
         (empty? storage))
  (equiv [this that]
         (if (or (symbol? that)
                 (= (type that) Metaconstant)
                 (= that unbound-marker))
           (.equals this that)
           (throw (user-error
                   (str "Metaconstants (" (.name this) ") can't be compared for equality with " (pr-str that) ".")
                   "If you have a compelling case for equality, please create an issue:"
                   ecosystem/issues-url)))))

(defn merge-metaconstants [^Metaconstant mc1 ^Metaconstant mc2]
  (Metaconstant. (.name mc1) (merge (.storage mc1) (.storage mc2))))

(defmethod print-method Metaconstant [^Metaconstant o ^java.io.Writer w]
  (print-method (.name o) w))


(defn define-metaconstants [form]
  (let [metaconstants (filter metaconstant-symbol? (tree-seq coll? seq form))]
    (doseq [metaconstant metaconstants]
      (intern *ns* metaconstant (Metaconstant. metaconstant {})))
    metaconstants))

(def #^:dynamic #^:private *metaconstant-counts*)

(defmacro with-fresh-generated-metaconstant-names [& forms]
  `(binding [*metaconstant-counts* (atom {})]
     ~@forms))

(defn metaconstant-for-form [[function-symbol & _ :as inner-form]]
  (let [swap-fn (fn [current-value function-symbol]
                  (assoc current-value function-symbol ((fnil inc 0) (current-value function-symbol))))
        number ((swap! *metaconstant-counts* swap-fn function-symbol)
                  function-symbol)]
    (symbol (format "...%s-value-%s..." (name function-symbol) number))))