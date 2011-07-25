(ns midje.util.namespace
  (:use midje.util.treelike)
  (:require [clojure.zip :as zip]))


(defmulti namespacey-match (fn [symbols treelike] (tree-variant treelike)))

(defmethod namespacey-match :zipper [symbols loc]
   (namespacey-match symbols (zip/node loc)))

(defmethod namespacey-match :form [symbols node]
  (let [base-names (map name symbols)
        qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
                                (map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str node))))


  
