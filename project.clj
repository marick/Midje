(defproject midje "1.3-alpha5"
  :description "A TDD library for Clojure, with an emphasis on mocks"
  :url "https://github.com/marick/Midje"
  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1],[1.3.0]"]
;  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1]"]
;                 [ordered "0.3.0"]  ; Not yet updated for Clojure 1.3.
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.unify "0.5.1"]
                 [utilize "0.1.2"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [slamhound "1.1.1"]]
  ;; automatically detect when your :dependencies key changes and run
  ;; lein deps behind the scenes when necessary.
  :checksum-deps true)

