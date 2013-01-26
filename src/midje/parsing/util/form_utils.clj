(ns ^{:doc "Utility functions dealing with checking or tranforming forms or zippers."}
  midje.parsing.util.form-utils
  (:use midje.clojure.core
        [utilize.seq :only (first-truthy-fn)])
  (:require [clojure.zip :as zip]))


(defn tree-variant [treelike]
  (letfn [(is-zipper? [treelike]
            (:zip/make-node (meta treelike)))]
    (if (is-zipper? treelike) :zipper :form)))
