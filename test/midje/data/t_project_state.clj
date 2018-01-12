(ns midje.data.t-project-state
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.data.project-state :refer :all :as project-state]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.bultitude :as tude]
            [clojure.java.io :as io]))


;;; Directory structure

(fact "unglob-partial-namespaces returns namespace symbols"
  (fact "from symbols or strings"
    (unglob-partial-namespaces ["explicit-namespace1"]) => ['explicit-namespace1]
    (unglob-partial-namespaces ['explicit-namespace2]) => ['explicit-namespace2])

  (fact "can 'unglob' wildcards"
    (prerequisites (ecosystem/leiningen-paths) => ["test" "src"]
                   (tude/classify-dir-entries "test/ns/foo") => [{:status :contains-namespace
                                                                   :namespace-symbol 'ns.foo.t-1}
                                                                 {:status :contains-namespace
                                                                   :namespace-symbol 'ns.foo.t-2}]
                   (tude/classify-dir-entries "src/ns/foo") => [{:status :contains-namespace
                                                                  :namespace-symbol 'ns.foo.bar}])

    (unglob-partial-namespaces ["ns.foo.*"]) => '[ns.foo.t-1 ns.foo.t-2 ns.foo.bar]
    (unglob-partial-namespaces ['ns.foo.* ]) => '[ns.foo.t-1 ns.foo.t-2 ns.foo.bar]))


;;; Working with modification times and dependencies

(fact "The files to load can be used to find a modification time"
  (against-background (file-modification-time ..file1..) => 222
    (file-modification-time ..file2..) => 3333)

  (let [empty-tracker {time-key 11
                       load-key []
                       filemap-key {..file1.. ..ns1..
                                    ..file2.. ..ns2..}}
        tracker-with-changes (assoc empty-tracker load-key [..ns1.. ..ns2..])
        tracker-with-deleled-ns (assoc empty-tracker load-key [..ns1..]
                                                     unload-key [..ns1.. ..ns2..]
                                                     deps-key {:dependents {..ns1.. #{..ns2.. ..ns3..}}})]

    (latest-modification-time empty-tracker) => 11
    (latest-modification-time tracker-with-changes) => 3333

    (prepare-for-next-scan empty-tracker) => (contains {time-key 11, unload-key [], load-key []})
    (prepare-for-next-scan tracker-with-changes) => (contains {time-key 3333, unload-key [], load-key []})
    (prepare-for-next-scan tracker-with-deleled-ns) => (contains {deps-key {:dependents {..ns1.. #{..ns3..}}}})))


(fact "a dependents cleaner knows how to remove namespaces that depend on a namespace"
  (let [tracker {deps-key {:dependents {..ns1.. [..ns2..]
                                        ..ns2.. [..ns3..]
                                        ..ns3.. []}}}
        cleaner (mkfn:clean-dependents tracker)]
    (cleaner ..ns1.. [..ns2.. ..ns3..]) => empty?
    (cleaner ..ns2.. [..ns1.. ..ns3..]) => [..ns1..]
    (cleaner ..ns3.. [..ns1..]) => [..ns1..]))


(def cleaner) ; standin for the calculated dependency cleaner
(def failure-record (atom {}))
(defn record-failure [ns throwable]
  (swap! failure-record (constantly {:ns ns, :throwable throwable})))


(fact "A namespace list can be loaded, obeying dependents"
  (#'project-state/require-namespaces! [] record-failure cleaner) => anything

  (#'project-state/require-namespaces! [..ns1.. ..ns2..] record-failure cleaner) => anything
  (provided
    (#'project-state/load-failure ..ns1..) => nil
    (#'project-state/load-failure ..ns2..) => nil)

  (#'project-state/require-namespaces! [..ns1.. ..ns2..] record-failure cleaner) => anything
  (provided
    (#'project-state/load-failure ..ns1..) => nil
    (#'project-state/load-failure ..ns2..) => nil)

  (let [throwable (Error.)]
    (#'project-state/require-namespaces! [..ns1.. ..ns2.. ..ns3..] record-failure cleaner) => anything
    (provided
      (#'project-state/load-failure ..ns1..) => throwable
      (cleaner ..ns1.. [..ns2.. ..ns3..]) => [..ns3..]
      (#'project-state/load-failure ..ns3..) => nil)
    @failure-record => {:ns ..ns1.. :throwable throwable}))

