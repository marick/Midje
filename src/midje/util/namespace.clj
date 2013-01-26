(ns midje.util.namespace
  (:use midje.clojure.core
        midje.parsing.util.form-utils)
  (:require [clojure.zip :as zip]))


(defmulti matches-symbols-in-semi-sweet-or-sweet-ns? (fn [_symbols_ treelike] (tree-variant treelike)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :zipper [symbols loc]
   (matches-symbols-in-semi-sweet-or-sweet-ns? symbols (zip/node loc)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :form [symbols node]
  (let [base-names       (map name symbols)
        semi-sweet-names (map #(str "midje.semi-sweet/" %) base-names)
        sweet-names      (map #(str "midje.sweet/" %) base-names)]
    (some #(= % (str node)) (concat base-names semi-sweet-names sweet-names))))

(defn semi-sweet-keyword? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect fake not-called data-fake) loc))


