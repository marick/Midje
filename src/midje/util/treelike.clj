(ns midje.util.treelike
  (:require [clojure.zip :as zip]))
  
(defn- is-zipper? [treelike]
  (:zip/make-node (meta treelike)))

(defn tree-variant [treelike]
  (if (is-zipper? treelike) :zipper :form))

