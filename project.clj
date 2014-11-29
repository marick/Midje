(defproject midje "1.6.4-SNAPSHOT"
  :description "A TDD library for Clojure that supports top-down ('mockish') TDD, encourages readable tests, provides a smooth migration path from clojure.test, balances abstraction and concreteness, and strives for graciousness."
  :url "https://github.com/marick/Midje"
  :pedantic? :warn
  :dependencies [[org.clojure/clojure "1.5.1"]
                 ;; upgrading to 1.3.2 produces record-type output for ordered maps
                 [ordered "1.2.0" :exclusions [org.clojure/clojure]]
                 [org.clojure/math.combinatorics "0.0.8"]
                 ;; Changing following to 0.5.6 makes a t_unify test fail.
                 [org.clojure/core.unify "0.5.2" :exclusions [org.clojure/clojure]]
                 [clj-time "0.8.0"]
                 [utilize "0.2.3" :exclusions [org.clojure/clojure]]
                 [colorize "0.1.1" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.macro "0.1.5"]
                 [dynapath "0.2.0"]
                 [difform "1.1.2"]
                 [org.tcrawley/dynapath "0.2.3"]
                 [swiss-arrows "1.0.0"]
                 [org.clojure/tools.namespace "0.2.7"]
                 [slingshot "0.12.1"]
                 [commons-codec/commons-codec "1.10"]
                 ;; upgrade to 0.6.5 causes a reducers-not-found error
                 [gui-diff "0.5.0"]]
  :profiles {:dev {:dependencies [[slamhound "1.5.5"]
                                  [jonase/kibit "0.0.8"]
                                  [prismatic/plumbing "0.3.5"]
                                  [jonase/eastwood "0.2.0"]]
                   :plugins [[lein-midje "3.1.4-SNAPSHOT"]]}
             :test-libs {:dependencies [[prismatic/plumbing "0.3.5"]]}
             :1.3 [:test-libs {:dependencies [[org.clojure/clojure "1.3.0"]]}]
             :1.4 [:test-libs {:dependencies [[org.clojure/clojure "1.4.0"]]}]
             :1.5.0 [:test-libs {:dependencies [[org.clojure/clojure "1.5.0"]]}]
             :1.5.1 [:test-libs {:dependencies [[org.clojure/clojure "1.5.1"]]}]
             :1.6 [:test-libs {:dependencies [[org.clojure/clojure "1.6.0"]
                                              [org.clojure/tools.nrepl "0.2.3"]]}]
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
  :aliases {"compatibility" ["with-profile" "1.3:1.4:1.5.0:1.5.1:1.6" "test"]}

  ;; For Clojure snapshots
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"
                 "stuartsierra-releases" "http://stuartsierra.com/maven2"})
