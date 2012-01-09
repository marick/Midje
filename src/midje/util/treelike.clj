(ns midje.util.treelike
  (:require [clojure.zip :as zip]))
  
(defn tree-variant [treelike]
  (letfn [(is-zipper? [treelike]
            (:zip/make-node (meta treelike)))]
    (if (is-zipper? treelike) :zipper :form)))

