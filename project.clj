(def common-deps '[[ordered "1.2.0" :exclusions [org.clojure/clojure]]
                   [org.clojure/math.combinatorics "0.0.1"]
                   [org.clojure/algo.monads "0.1.0"]
                   [org.clojure/core.unify "0.5.2" :exclusions [org.clojure/clojure]]
                   [utilize "0.2.3" :exclusions [org.clojure/clojure]]
                   [colorize "0.1.1" :exclusions [org.clojure/clojure]]
                   [org.clojure/tools.macro "0.1.1"]
                   [org.clojure/core.incubator "0.1.0"]
                   [swiss-arrows "0.1.0"]])

(defproject midje "1.4.0"
  :description "A TDD library for Clojure that supports top-down ('mockish') TDD, encourages readable tests, provides a smooth migration path from clojure.test, balances abstraction and concreteness, and strives for graciousness."
  :url "https://github.com/marick/Midje"
  :dependencies ~(cons '[org.clojure/clojure "1.4.0"]
                       common-deps)

  :multi-deps {"1.2.0" [[org.clojure/clojure "1.2.0"]]
               "1.2.1" [[org.clojure/clojure "1.2.1"]]
               "1.3.0" [[org.clojure/clojure "1.3.0"]]
               "1.4.0" [[org.clojure/clojure "1.4.0"]]
               "1.5.0" [[org.clojure/clojure "1.5.0-SNAPSHOT"]]
               :all ~common-deps }
  :dev-dependencies [[slamhound "1.2.0"]
                     [jonase/kibit "0.0.3"]
                     [jonase/eastwood "0.0.2"]
                     [com.stuartsierra/lazytest "1.2.3"]]

  ;; automatically detects when your :dependencies key changes and runs
  ;; lein deps behind the scenes when necessary.
  :checksum-deps true
;  :warn-on-reflection true      ;; turn on to check production code for reflection

  ;; For Clojure snapshots
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"
                 "stuartsierra-releases" "http://stuartsierra.com/maven2"})
