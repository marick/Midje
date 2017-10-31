(ns midje.data.t-project-state
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.data.project-state :refer :all]
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

(def cleaner) ; standin for the calculated dependency cleaner
(def failure-record (atom {}))
(defn record-failure [ns throwable]
  (swap! failure-record (constantly {:ns ns, :throwable throwable})))


#_(fact "A namespace list can be loaded, obeying dependents"
  (#'require-namespaces! [] record-failure) => anything

  (#'require-namespaces! [..ns1.. ..ns2..] record-failure) => anything
  (provided
    (require ..ns1.. :reload) => nil
    (require ..ns2.. :reload) => nil)

  (#'require-namespaces! [..ns1.. ..ns2..] record-failure) => anything
  (provided
    (require ..ns1.. :reload) => nil
    (require ..ns2.. :reload) => nil)



  (let [throwable (Error.)]
    (#'require-namespaces! [..ns1.. ..ns2.. ..ns3..] record-failure) => anything
    (provided
      (require ..ns1.. :reload) =throws=> throwable
      (require ..ns3.. :reload) => nil)
    @failure-record => {:ns ..ns1.. :throwable throwable}))

