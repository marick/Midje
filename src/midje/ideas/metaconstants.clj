;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.metaconstants
  (:use [midje.util.form-utils :only [quoted? translate form-first?]]
        [midje.util.zip :only [skip-down-then-rightmost-leaf]])
  (:require [clojure.zip :as zip]))


(defn metaconstant-symbol? [symbol-or-form]
  (and (symbol? symbol-or-form)
       (re-matches #"^\.+.+\.+" (name symbol-or-form))))

(defn define-metaconstants [form]
  (let [metaconstants (filter metaconstant-symbol? (tree-seq coll? seq form))]
    (doseq [metaconstant metaconstants]
      (intern *ns* metaconstant (symbol metaconstant)))
    metaconstants))

(def *metaconstant-counts*)

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

;; Treating metaconstants as implementing ILookup

(defn meta-get [metaconstant key & rest]
  (throw (Error. "meta-get has no implementation. It is used to fake lookup on metaconstants.")))


(defn key-first-lookup? [loc]
  (let [tree (zip/node loc)]
    (and 
         (>= (count tree) 2)
         (keyword? (first tree))
         (metaconstant-symbol? (second tree)))))

(defn key-first-transformation [loc]
  (let [ [key meta & rest] (zip/node loc)]
    (zip/replace loc `(meta-get ~meta ~key ~@rest))))

(defn meta-first-lookup? [loc]
  (let [tree (zip/node loc)]
    (and 
         (>= (count tree) 2)
         (metaconstant-symbol? (first tree)))))

(defn meta-first-transformation [loc]
  (let [ [key meta & rest] (zip/node loc)]
    (zip/replace loc `(meta-get ~key ~meta ~@rest))))

  

(defn metaconstant-lookup-transform [forms]
  (translate forms
             (complement zip/branch?) identity
             quoted? skip-down-then-rightmost-leaf
             key-first-lookup? key-first-transformation
             meta-first-lookup? meta-first-transformation))
