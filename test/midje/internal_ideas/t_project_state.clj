(ns midje.internal-ideas.t-project-state
  (:use [midje.sweet]
        [clojure.pprint]
        [midje.test-util]
        [midje.internal-ideas.project-state])
  (:require [midje.util.ecosystem :as ecosystem]))


;;; Directory structure

(ecosystem/when-1-3+

  (fact "can find paths to load from project.clj"
    (fact "if it exists"
      (directories) => ["/test1" "/src1"]
      (provided (leiningen.core.project/read) => {:test-paths ["/test1"]
                                                  :source-paths ["/src1"]}))
    
    (fact "and provides a default if it does not"
      (directories) => ["test"]
      (provided (leiningen.core.project/read)
                =throws=> (new java.io.FileNotFoundException))))
  
  
  (fact "unglob-partial-namespaces returns namespace symbols"
    (fact "from symbols or strings"
      (unglob-partial-namespaces ["explicit-namespace1"]) => ['explicit-namespace1]
      (unglob-partial-namespaces ['explicit-namespace2]) => ['explicit-namespace2])
    
    (fact "can 'unglob' wildcards"
      (unglob-partial-namespaces ["ns.foo.*"]) => '[ns.foo.bar ns.foo.baz]
      (provided (bultitude.core/namespaces-on-classpath :prefix "ns.foo.")
                => '[ns.foo.bar ns.foo.baz])
      
      (unglob-partial-namespaces ['ns.foo.*]) => '[ns.foo.bar ns.foo.baz]
      (provided (bultitude.core/namespaces-on-classpath :prefix "ns.foo.")
                => '[ns.foo.bar ns.foo.baz])))
  
;;; Working with modification times and dependencies

 (fact "working with the tools.namespace tracker structure"
   (against-background (file-modification-time ..file1..) => 222
                       (file-modification-time ..file2..) => 3333)
   (let [core-tracker
         {time-key 11
          unload-key [..ns1.. ..ns2..]
          load-key [..ns1.. ..ns2..]
          filemap-key {..file1.. ..ns1..
                       ..file2.. ..ns2..}}]
     (facts "modification times"
       (latest-modification-time core-tracker) => 3333
       (latest-modification-time (assoc core-tracker load-key [])) => 11
       (autotest-augment-tracker core-tracker) => (contains {next-time-key 3333}))

     (facts "creating the tracker appropriate for the next check"
       (autotest-next-tracker (autotest-augment-tracker core-tracker))
       => (contains {time-key 3333, unload-key [], load-key []})))


   (fact "a namespace and all that depend on it can be removed"
     (let [dependency-tracker {load-key [..core-ns.. ..depends-on-core.. ..does-not-depend..]
                               deps-key {:dependents {..core-ns.. #{..depends-on-core..}}}}]
       (without-first-to-load-and-dependents dependency-tracker) => (contains {load-key [..does-not-depend..]}))))
 )
                              
