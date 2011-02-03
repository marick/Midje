(ns behaviors.folded-prerequisites-and-namespaces.using-namespace
  (:require [behaviors.folded-prerequisites-and-namespaces.source-namespace :as x]))

(defn c [n]
  (x/b (x/a n)))
