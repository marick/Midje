(ns behaviors.folded-prerequisites-and-namespaces.t-using-namespace
  (:require [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [behaviors.folded-prerequisites-and-namespaces.using-namespace :refer :all]
            [behaviors.folded-prerequisites-and-namespaces.source-namespace :as x]))

(deftest test-something
  (fact
    (c 1) => 3
    (provided
      (x/b (x/a 1)) => 3)))
