(ns behaviors.folded-prerequisites-and-namespaces.t-using-namespace
  (:use [clojure.test]
        [midje.sweet]
        [behaviors.folded-prerequisites-and-namespaces.using-namespace])
  (:require [behaviors.folded-prerequisites-and-namespaces.source-namespace :as x]))

(deftest test-something
  (fact
    (c 1) => 3
    (provided
      (x/b (x/a 1)) => 3)))
