(defproject midje "1.5-SNAPSHOT"
  :description "A TDD library for Clojure that supports top-down ('mockish') TDD, encourages readable tests, provides a smooth migration path from clojure.test, balances abstraction and concreteness, and strives for graciousness."
  :url "https://github.com/marick/Midje"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [ordered "1.2.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.1"]
                 [org.clojure/algo.monads "0.1.0"]
                 [org.clojure/core.unify "0.5.2" :exclusions [org.clojure/clojure]]
                 [utilize "0.2.3" :exclusions [org.clojure/clojure]]
                 [colorize "0.1.1" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.macro "0.1.1"]
                 [org.clojure/core.incubator "0.1.0"]
                 [bultitude "0.1.7"]
                 [leiningen-core "2.0.0-preview10"]
                 [swiss-arrows "0.1.0"]
                 [org.clojure/tools.namespace "0.2.2"]
                 [slingshot "0.10.3"]
                 [commons-codec/commons-codec "1.7"]
                 [gui-diff "0.3.9"]]
  :profiles {:dev {:dependencies [[slamhound "1.2.0"]
                                  [jonase/kibit "0.0.3"]
                                  [jonase/eastwood "0.0.2"]]
                   :plugins [[lein-midje "3.0-SNAPSHOT"]]}
             :1.2.0 {:dependencies [[org.clojure/clojure "1.2.0"]]}
             :1.2.1 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.0-RC4"]]}
             ;; The following profile can be used to check that `lein with-profile`
             ;; profiles are obeyed. Note that profile `:test-paths` *add on* to the
             ;; defaults.
             :test-test-paths {:test-paths ["test-test-paths"]}}
  :resource-paths ["test-resources"]
  :license {:name "The MIT License (MIT)"
            :url "http://opensource.org/licenses/mit-license.php"
            :distribution :repo}
  :mailing-list {:name "Midje"
                 :subscribe "https://groups.google.com/forum/?fromgroups#!forum/midje"}
  :aliases {"compatibility" ["with-profile" "1.3:1.4:1.5" "test"]
            "compatibility12" ["with-profile" "1.2.0:1.2.1:1.3:1.4:1.5" "test"]}
  
  ;; For Clojure snapshots
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"
                 "stuartsierra-releases" "http://stuartsierra.com/maven2"})
