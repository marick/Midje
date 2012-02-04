(defproject midje "1.3.2-alpha1"
  :description "A TDD library for Clojure that supports top-down ('mockish') TDD, encourages readable tests, provides a smooth migration path from clojure.test, balances abstraction and concreteness, and strives for graciousness."
  :url "https://github.com/marick/Midje"
  :dependencies [[org.clojure/clojure "[1.2.0],[1.2.1],[1.3.0]"]
                 [ordered "1.0.0"]
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.unify "0.5.1"]
                 [utilize "0.2.2"]
                 [colorize "0.1.1"]
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/core.incubator "0.1.0"]
                 [clj-stacktrace-alex "0.2.5"]
                 [org.clojure/clojurescript "0.0-927"]]
  :dev-dependencies [[slamhound "1.2.0"]
                     [com.intelie/lazytest "1.0.0-SNAPSHOT" :exclusions [swank-clojure]]]
  ;; automatically detects when your :dependencies key changes and runs
  ;; lein deps behind the scenes when necessary.
  :checksum-deps true
;;  :warn-on-reflection true      ;; turn on to check production code for reflection

  ;; For Clojure snapshots
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"})
