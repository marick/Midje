(defproject midje "1.9.2-alpha2"
  :description "A TDD library for Clojure that supports top-down ('mockish') TDD, encourages readable tests, provides a smooth migration path from clojure.test, balances abstraction and concreteness, and strives for graciousness."
  :url "https://github.com/marick/Midje"
  :pedantic? :warn
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [marick/suchwow "6.0.2" :exclusions [org.clojure/clojure org.clojure/clojurescript]]
                 [org.clojure/math.combinatorics "0.1.4"]
                 [io.aviso/pretty "0.1.34"]
                 [org.clojure/core.unify "0.5.7" :exclusions [org.clojure/clojure]]
                 [org.clojure/test.check "0.9.0"]
                 [clj-time "0.14.2" :exclusions [org.clojure/clojure]]
                 [colorize "0.1.1" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.macro "0.1.5"]
                 [org.tcrawley/dynapath "1.0.0"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [flare "0.2.9" :exclusions [org.clojure/clojure]]
                 [slingshot "0.12.2"]
                 [mvxcvi/puget "1.0.2" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:dependencies [[prismatic/plumbing "0.5.5"]]
                   :plugins [[lein-midje "3.2.1"]
                             [lein-ancient "0.6.14" :exclusions [com.fasterxml.jackson.core/jackson-databind
                                                                 com.fasterxml.jackson.core/jackson-core]]]}
             :test-libs {:dependencies [[prismatic/plumbing "0.5.5"]]}
             :1.7 [:test-libs {:dependencies [[org.clojure/clojure "1.7.0"]]}]
             :1.8 [:test-libs {:dependencies [[org.clojure/clojure "1.8.0"]]}]
             :1.9 [:test-libs {:dependencies [[org.clojure/clojure "1.9.0"]]}]
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
  :deploy-repositories [["releases" :clojars]
                        ["snapshots" :clojars]]

  :aliases {"compatibility" ["with-profile" "1.7:1.8:1.9" "midje" ":config" ".compatibility-test-config"]
            "travis" ["with-profile" "1.7:1.8:1.9" "midje"]}

  ;; For Clojure snapshots
  :repositories {"sonatype-oss-public" "https://oss.sonatype.org/content/groups/public/"
                 "stuartsierra-releases" "https://stuartsierra.com/maven2"})
