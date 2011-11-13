;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.metaconstants
  (:use [midje.util.form-utils :only [quoted? translate-zipper]]
        [midje.util.zip :only [skip-down-then-rightmost-leaf]]
        [midje.util.thread-safe-var-nesting :only [unbound-marker]]
        [midje.util.exceptions :only [user-error]]
        [midje.error-handling.monadic :only [user-error-report-form validate]]
        [utilize.seq :only (find-first)]
        [midje.util.old-clojure-contrib.core :only (-?>>)])
  (:require [clojure.zip :as zip]
            [midje.util.ecosystem :as ecosystem]))

(defn- normalize [re metaconstant-format mc-symbol]
  (-?>> mc-symbol name (re-matches re) second (format metaconstant-format)))

(def ^{:private true} dot-metaconstant  (partial normalize #"^\.+(.+?)\.+$" "...%s..."))
(def ^{:private true} dash-metaconstant (partial normalize #"^-+(.+?)-+$"   "---%s---"))

(defn- normalized-metaconstant
  "Turns '..m. to \"...m...\""
  [mc-symbol]
  (->> mc-symbol ((juxt dot-metaconstant dash-metaconstant)) (find-first identity)))  

(defn metaconstant-symbol? [symbol-or-form]
  (and (symbol? symbol-or-form)                                               
       (-> symbol-or-form normalized-metaconstant not not)))

(deftype Metaconstant [name storage]
  Object
  (toString [this]
            (.toString (.name this)))
  (equals [this that]
         (if (instance? (class this) that)
           (= (normalized-metaconstant (.name this)) (normalized-metaconstant (.name that)))
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

(defmethod print-method Metaconstant [o ^java.io.Writer w]
  (print-method (.name o) w))


(defn define-metaconstants [form]
  (let [metaconstants (filter metaconstant-symbol? (tree-seq coll? seq form))]
    (doseq [metaconstant metaconstants]
      (intern *ns* metaconstant (Metaconstant. metaconstant {})))
    metaconstants))

(def ^{:dynamic true} *metaconstant-counts*)

(defmacro with-fresh-generated-metaconstant-names [& forms]
  `(binding [*metaconstant-counts* (atom {})]
     ~@forms))

(defn metaconstant-for-form [[function-symbol & _ :as inner-form]]
  (let [swap-fn (fn [current-value function-symbol]
                  (if (current-value function-symbol)
                    (assoc current-value function-symbol
                           (inc (current-value function-symbol)))
                    (assoc current-value function-symbol 1)))
        number ((swap! *metaconstant-counts* swap-fn function-symbol)
                function-symbol)]
    (symbol (format "...%s-value-%s..." (name function-symbol) number))))

        
