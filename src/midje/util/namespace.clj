(ns midje.util.namespace
  (:use midje.util.treelike)
  (:require [clojure.zip :as zip]))


(defmulti matches-symbols-in-semi-sweet-or-sweet-ns? (fn [symbols treelike] (tree-variant treelike)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :zipper [symbols loc]
   (matches-symbols-in-semi-sweet-or-sweet-ns? symbols (zip/node loc)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :form [symbols node]
  (let [base-names (map name symbols)
        qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
                                (map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str node))))


  
