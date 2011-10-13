(defproject midje "1.3-alpha4"
  :description "A TDD library for Clojure, with an emphasis on mocks"
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"}
  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1],[1.3.0]"]
;                 [ordered "0.3.0"]  ; Not yet updated for Clojure 1.3.
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.unify "0.5.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
                     [slamhound "1.1.1"]])

