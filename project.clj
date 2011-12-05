(defproject midje "1.3.1-SNAPSHOT"
  :description "A TDD library for Clojure, with an emphasis on mocks"
  :url "https://github.com/marick/Midje"
  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1],[1.3.0]"]
;  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1]"]
                 [ordered "1.0.0"]
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.unify "0.5.1"]
                 [utilize "0.1.3"]
                 [colorize "0.1.1"]
                 [org.clojure/tools.macro "0.1.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [slamhound "1.1.1"]
                     [com.intelie/lazytest "1.0.0-SNAPSHOT"]]
  ;; automatically detects when your :dependencies key changes and runs
  ;; lein deps behind the scenes when necessary.
  :checksum-deps true)

