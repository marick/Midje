(ns midje.util.t-ecosystem
  (:require [midje.sweet :refer :all]
            [midje.util.ecosystem :refer :all]
            [midje.test-util :refer :all]))

(fact "can find paths to load from project.clj"
  (against-background (around :facts (around-initial-paths ?form)))
  (fact "if it exists"
    (with-redefs [clojure.core/load-file
                  (fn [name] (eval '(defproject scratch "1.0.0-SNAPSHOT"
                                      :source-paths ["src1"]
                                      :test-paths ["test1"])))]
      (leiningen-paths) => ["test1" "src1"]))

  (fact "and provides a default if it does not"
    (leiningen-paths) => ["test"]
    (provided
      (load-file "project.clj") =throws=> (new java.io.FileNotFoundException)))

  (fact "except that lein-midje can explicitly set the value"
    (set-leiningen-paths! {:test-paths ["t"] :source-paths ["s"]})
    (leiningen-paths) => ["t" "s"])

  (fact "note that test paths come first"
    (set-leiningen-paths! (sorted-map :source-paths ["after"] :test-paths ["before"]))
    (leiningen-paths) => ["before" "after"]))

