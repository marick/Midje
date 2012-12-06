(ns midje.t-repl
  (:use midje.repl
        [clojure.pprint]
        [midje.test-util])
  (:require [midje.internal-ideas.compendium :as compendium]
            [midje.clojure-test-facade :as ctf]
            [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.reporting.level-defs :as deflevels]
            [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]
            midje.util))

(ecosystem/when-1-3+

(midje.util/expose-testables midje.repl)

;;;; === Util

 (defn add-fact
   [& args]
   (compendium/record-fact-existence!
    (with-meta (gensym)
      (merge {:midje/namespace 'midje.sweet :midje/name "with a name"
              :a-property true :midje/source '(code)}
             (apply hash-map args)))))

 (defn names [fact-functions]
   (map fact-name fact-functions))

 ;; Use this when using fetch-facts to test other functions.
 ;; Makes it easier to see at a glance what the test is about.
 (defn fetched-names [& args]
   (names (apply fetch-facts args)))

 (defn compendium-count [arg]
   (count (fetch-facts arg)))


;;;; === PART 1: Loading facts from files

(confirming-cumulative-totals-not-stepped-on
 (without-changing-cumulative-totals
  (fact "load-facts" :check-only-at-load-time
    (fact "Can load namespace by symbol"
      (forget-facts :all)
      (load-facts 'midje.t-repl-helper :print-no-summary)
      (count (fetch-facts)) => 2)

    (fact "Can load namespace by its object"
      (forget-facts :all)
      (load-facts (the-ns 'midje.t-repl-helper) :print-no-summary)
      (count (fetch-facts)) => 2)

    (fact "Loading a file erases the previous version"
      (forget-facts :all)
      ;; Put in a dummy fact so we can see that it's erased.
      (add-fact :midje/namespace 'midje.t-repl-helper
                :midje/name "FAKE")
      (count (fetch-facts)) => 1
      (load-facts (the-ns 'midje.t-repl-helper) :print-no-summary)
      (count (fetch-facts)) => 2
      (filter #(= (fact-name %) "FAKE") (fetch-facts :all)) => empty?)
  
    (fact "metadata filters are obeyed"
      (load-facts 'midje.t-repl-helper :non-featherian :print-no-summary)
      (map fact-name (fetch-facts :all)) => ["a non-featherian test"]

      (load-facts :print-no-summary 'midje.t-repl-helper "simple")
      (map fact-name (fetch-facts)) => ["a simple test"])

    (fact "the :all argument"
      (load-facts :print-no-summary :all "simple") => anything
      (provided
        (#'midje.repl/project-namespaces) => ['midje.t-repl-helper])
      (map fact-name (fetch-facts :all)) => ["a simple test"])

    (fact "no arguments repeats previous arguments"
      (load-facts 'midje.t-repl-helper :non-featherian :print-no-summary)
      (forget-facts :all)
      (load-facts)
      (map fact-name (fetch-facts)) => ["a non-featherian test"])

    (future-fact "repetition not affected by intervening check-facts"
      (load-facts 'midje.t-repl-helper :non-featherian :print-no-summary)
      (check-facts ':all :print-no-summary)
      (forget-facts :all)
      (load-facts)
      (map fact-name (fetch-facts :all)) => ["a non-featherian test"])

    (fact "load-facts sets up default arguments for fetch-facts"
      (load-facts :all "simple" :print-no-summary) => anything
      (provided
        (#'midje.repl/project-namespaces) => ['midje.t-repl-helper])
      ;; The double @ is because of importing testables.
      @@fetch-default-args => (just [:all "simple" :print-no-summary]
                                    :in-any-order)

      (load-facts 'midje.t-repl-h* :non-featherian :print-no-summary) => anything
      (provided (#'midje.repl/unglob-partial-namespaces ['midje.t-repl-h*])
                => '[midje.t-repl-helper])
      @@fetch-default-args => ['midje.t-repl-helper :non-featherian :print-no-summary]))
 )


;;;; ==== PART 2: Working with loaded facts

 
                                ;;; Fetching facts
 (compendium/fresh!)

;;; Note that facts must be returned in alphetical order by namespace,
;;; then by creation order within namespace.
 (add-fact :midje/namespace 'midje.ideas.facts :midje/name "f - second - midje.ideas.facts")
 (add-fact :midje/namespace 'midje.t-repl :midje/name      "m - fourth - midje.t-repl")
 (add-fact :midje/namespace 'midje.repl :midje/name        "p - third - midje.repl" )
 (add-fact :midje/namespace 'midje.t-repl :midje/name      "a - fifth - midje.t-repl")
 (add-fact :midje/namespace 'midje.config :midje/name      "z - first - integration" :integration true)

 (fact :check-only-at-load-time
   ;; In some of these, I check that changing the argument to fetch-facts affects the default.
   (let [expected ["m - fourth - midje.t-repl" "a - fifth - midje.t-repl"]]
     (names (fetch-facts *ns*)) => expected
     (names (fetch-facts)) => expected
     (names (fetch-facts)) => expected)

   (let [expected ["f - second - midje.ideas.facts"]]
     (names (fetch-facts 'midje.ideas.facts)) => expected
     (names (fetch-facts)) => expected)

   ;; if explicit namespace order is given, it's obeyed
   (let [expected ["m - fourth - midje.t-repl" "a - fifth - midje.t-repl" "z - first - integration" ]]
     (names (fetch-facts 'midje.t-repl 'midje.config)) => expected
     (names (fetch-facts)) => expected)

   (let [expected ["z - first - integration" "f - second - midje.ideas.facts" "p - third - midje.repl" "m - fourth - midje.t-repl" "a - fifth - midje.t-repl"]]
     (names (fetch-facts :all)) => expected
     (names (fetch-facts)) => expected)

   (let [expected ["z - first - integration" "f - second - midje.ideas.facts"]]
     (names (fetch-facts :all "n")) => expected
     (names (fetch-facts)) => expected)
   
   (names (fetch-facts 'midje.ideas.facts #"q")) => []
   (names (fetch-facts :all :integration)) => ["z - first - integration"]
   (names (fetch-facts *ns* :integration)) => []
   (names (fetch-facts :all #(= (:midje/name %) "p - third - midje.repl"))) => ["p - third - midje.repl"]
   (names (fetch-facts "midje.ideas.facts" "p - third - midje.repl" :all))
   => ["f - second - midje.ideas.facts" "p - third - midje.repl"]
   )

   


                                ;;; Forgetting facts

 ;; all of them
 (fact (* 2 2) => (+ 2 2)) ;; make sure one exists
 (fact :check-only-at-load-time (compendium-count :all) => pos?)
 (forget-facts :all)
 (fact :check-only-at-load-time (compendium-count :all) => zero?)

 ;; Explicit namespace argument
 (forget-facts :all)
 (add-fact :midje/namespace (ns-name *ns*))
 (add-fact :midje/namespace 'midje.sweet)
 (forget-facts *ns*)
 (fact :check-only-at-load-time
   (compendium-count *ns*) => zero?
   (compendium-count 'midje.sweet) => pos?)

 (forget-facts 'midje.sweet)
 (fact :check-only-at-load-time
   (compendium-count 'midje.sweet) => zero?)

;;; Filtering results by name
 (defn three-names []
   (forget-facts :all)
   ;; Adding in alphabetical order by namespace for clearer output
   (add-fact :midje/name "ghi" :midje/namespace 'midje.config)
   (add-fact :midje/name "bcdef" :midje/namespace 'midje.semi-sweet)
   (add-fact :midje/name "abcde" :midje/namespace 'midje.sweet))

 (three-names)
 (forget-facts 'midje.sweet "bcd")
 (fact :check-only-at-load-time
   (fetched-names :all) => ["ghi" "bcdef"])

 (three-names)
 (forget-facts :all #"bcd")
 (fact :check-only-at-load-time
   (fetched-names :all) => ["ghi"])

 ;; By metadata
 (forget-facts :all)
 (add-fact :midje/name "fred")
 (add-fact :midje/name "betty" :translation true)

 (forget-facts :all :translation)
 (fact :check-only-at-load-time
   (fetched-names :all) => ["fred"])

 ;; By predicate
 (forget-facts :all)
 (add-fact :midje/name "to be removed" :a true :b true)
 (add-fact :midje/name "bad-b" :a true :b false)
 (add-fact :midje/name "bad-a" :a false :b true)

 (forget-facts :all #(and (:a %) (:b %)))
 (fact :check-only-at-load-time
   (fetched-names :all) => (contains "bad-a" "bad-b" :in-any-order))


 (without-changing-cumulative-totals
  (fact "interactions between forget-facts and other functions"
    :check-only-at-load-time
    (fact "it can get its default from load-facts"
      (forget-facts :all)
      (add-fact :midje/namespace 'midje.ideas.facts :midje/name "not forgotten")
      (load-facts 'midje.t-repl-helper :print-nothing)
      (forget-facts)
      (map fact-name (compendium/all-facts<>)) => ["not forgotten"])
    
    (fact "it can get its default from check-facts"
      (forget-facts :all)
      (load-facts 'midje.t-repl-helper :print-nothing)
      (check-facts 'midje.t-repl-helper :non-featherian :print-nothing)
      (forget-facts)
      (map fact-name (compendium/namespace-facts<> 'midje.t-repl-helper))
      =not=> ["a non-featherian test"])
    
    (fact "forget-facts does not affect check-facts"
      (forget-facts :all)
      (load-facts 'midje.t-repl-helper :non-featherian :print-nothing)
      (add-fact :midje/namespace 'midje.ideas.facts)
      (forget-facts 'midje.ideas.facts)
      (map fact-name (fetch-facts)) => ["a non-featherian test"]))
  )
  



                                ;;; Checking facts

 (forget-facts :all)
 (fact "the-name" 1 => 1)
 (def the-fact (first (fetch-facts 'midje.t-repl)))

         ;;; Checking one fact

 (without-changing-cumulative-totals
  (ctf/zero-counters)
  (let [check-result (check-one-fact the-fact)]
    (fact :check-only-at-load-time
      (:pass (ctf/counters)) => 1
      check-result => true)))

 ;; Obeying print levels
 (let [text
       (config/with-augmented-config {:print-level :print-facts}
         (with-out-str (check-one-fact the-fact)))]
   (fact :check-only-at-load-time
     text => #"the-name"))

 ;; Obeying metadata filters
 (let [string-result ; will only be visible for facts defined at the repl.
       (config/with-augmented-config {:desired-fact? (with-meta (constantly false)
                                                       {:created-from ["desiderata"]})}
         
         (check-one-fact the-fact))]
   (fact :check-only-at-load-time
     string-result => #"ignored because of the current configuration"
     string-result => #"matching \[\"desiderata\"\]"))



        ;;; Checking multiple facts

 (without-externally-visible-changes (fact "error fact" 1 => 2))

 (without-changing-cumulative-totals

  (fact "you can give print-level arguments."
    :check-only-at-load-time
    (config/with-augmented-config {:print-level :print-nothing}
      ;; Despite above config.
      (with-out-str (check-facts 'midje.t-repl :print-namespaces)) => #"Namespace midje.t-repl")

    (fact "return values still hold, though"
      (check-facts 'midje.t-repl "the-name" :print-nothing) => true
      (check-facts 'midje.t-repl :print-nothing) => false))

  (fact "check-facts resets the counter"
    :check-only-at-load-time
    (check-facts 'midje.t-repl "the-name" :print-nothing)
    (:pass (ctf/counters)) => 1
    ;; no change below because of auto-zeroing
    (check-facts 'midje.t-repl "the-name" :print-nothing)
    (:pass (ctf/counters)) => 1)

  (fact "check-facts takes the usual arguments"
    :check-only-at-load-time
    ;; Most of the work is done by fetch-facts, but let's try a few
    (config/with-augmented-config {:print-level :print-nothing}
      (check-facts 'midje.config)
      (:pass (ctf/counters)) => 0
      (:fail (ctf/counters)) => 0

      (check-facts *ns*)
      (:pass (ctf/counters)) => 1
      (:fail (ctf/counters)) => 1

      (check-facts 'midje.t-repl "error fact")
      (:pass (ctf/counters)) => 0
      (:fail (ctf/counters)) => 1

      (check-facts :all :tinkerbell)
      (:pass (ctf/counters)) => 0
      (:fail (ctf/counters)) => 0))

  (fact "check-facts affects the next check-facts/fetch-facts"
    (check-facts 'midje.t-repl "the-name" :print-nothing) => true
    (check-facts) => true
    (map fact-name (fetch-facts)) => ["the-name"])

  ;; Old bug. A failure prevented further facts from being checked.
  (forget-facts :all)
  (without-externally-visible-changes
   (fact "1" (+ 1 2) => 3333)
   (fact "2" (+ 1 2) => 3))

  (fact "confirm the ordering of facts"
    :check-only-at-load-time
    (map fact-name (fetch-facts 'midje.t-repl)) => ["1" "2"])
  
  (fact "Both facts were checked"
    :check-only-at-load-time
    (check-facts 'midje.t-repl :print-nothing)
    (:pass (ctf/counters)) => 1
    (:fail (ctf/counters)) => 1)
  )

;;;; ==== PART 3: Rechecking the last-fact checked.

 (forget-facts :all)
 (fact "remembrance" (+ 1 1) => 2)

 (without-changing-cumulative-totals
  (config/with-augmented-config {:print-level :print-no-summary}
  
    (fact "Rechecking the fact resets the totals"
      :check-only-at-load-time
      (recheck-fact)
      (recheck-fact)
      (recheck-fact)
      (:pass (ctf/counters)) => 1
      (:fail (ctf/counters)) => 0)
    
    (fact "It also prints a summary"
      :check-only-at-load-time
      (config/with-augmented-config {:print-level :print-normally}
        (with-out-str (recheck-fact)) => #"All claims \(1\) have been confirmed"))

    (fact "It can take a print-level argument"
      :check-only-at-load-time
      (with-out-str (recheck-fact :print-namespaces)) => #"midje.t-repl")
    )

  (fact "You can ask for the fact without running it"
    :check-only-at-load-time
    (fact-name (last-fact-checked)) => "remembrance"
    (source-of-last-fact-checked) => '(fact "remembrance" (+ 1 1) => 2))
  )

;;;; ==== PART 4: Details on which facts get remembered


 (config/with-augmented-config {:print-level :print-no-summary}

         ;;; For check-facts

   ;; Nested facts are not included.
   (def inner-count (atom 0))
   (def outer-count (atom 0))

   (forget-facts :all)
   (fact "outer"
     1 => 1
     (swap! outer-count inc)
     (fact "inner"
       (swap! inner-count inc)
       2 => 2))
   
   (fact "only outer fact is available"
     :check-only-at-load-time
     (count (fetch-facts :all)) => 1
     (fact-name (first (fetch-facts :all))) => "outer")
   
   (fact
     :check-only-at-load-time
     (without-changing-cumulative-totals (check-facts :all))
     @outer-count => 2
     @inner-count => 2)
   
   
   ;; Facts with the same name redefine old versions.
   (forget-facts :all)
   
   (def run-count (atom 0))
   (fact "name"
     (swap! run-count inc))
   (fact "name" :redefinition
     @run-count => 1)
   
   (fact "Only the second fact now exists"
     :check-only-at-load-time
     (let [known-facts (fetch-facts :all)]
       (count known-facts) => 1
       (:redefinition (meta (first known-facts))) => true
       (without-changing-cumulative-totals (check-facts))
       @run-count => 1))
   
   ;; Redefinition of an unnamed fact to an identical body does
   ;; not produce copies.
   (forget-facts :all)
   (fact :note-that-the-metadata-does-not-matter (+ 1 1) => 2)
   (fact :some-other-metadata                    (+ 1 1) => 2)
   
   (fact "the old fact has been replaced"
     :check-only-at-load-time
     (count (fetch-facts :all)) => 1
     (map (comp :some-other-metadata meta) (fetch-facts :all)) => [true])
   
   
   ;; You can add a name to a fact.
   (forget-facts :all)
   (fact                         (+ 1 1) => 2)
   (fact "today we choose faces" (+ 1 1) => 2)
   
   (fact "there is now only a named fact"
     :check-only-at-load-time
     (count (fetch-facts :all)) => 1
     (map fact-name (fetch-facts :all)) => ["today we choose faces"])
   
   
   ;; An unnamed fact does not replace a named one with an identical body.
   ;; It becomes a new fact.
   (forget-facts :all)
   (fact "named" (+ 1 1) => 2)
   (fact         (+ 1 1) => 2)
   
   (fact "There are two facts"
     :check-only-at-load-time
     (count (fetch-facts :all)) => 2
     (map fact-name (fetch-facts :all)) => ["named" nil])

   
        ;;; For recheck-fact

   (def outer-run-count (atom 0))
    (def inner-run-count (atom 0))
    (fact outer
      (swap! outer-run-count inc)
      (+ 1 1) => 2
      (fact inner
        (swap! inner-run-count inc)
        (fact (- 1 1) => 0)))
    
    (fact "The last fact check is the outermost nested check"
      :check-only-at-load-time
      (without-changing-cumulative-totals (rcf))
      (fact-name (last-fact-checked)) => "outer"
      @outer-run-count => 2
      @inner-run-count => 2)

    ;; Multiple nested facts
    
    (def run-count (atom 0))
    (fact "outermost"
      (fact "inner 1"
        (swap! run-count inc))
      (fact "inner 2"
        (swap! run-count inc)))

    (fact :check-only-at-load-time
      (without-changing-cumulative-totals (recheck-fact))
      @run-count => 4)

    ;; Tabular facts count as nested facts

    (def run-count (atom 0))
    (tabular "tabular facts count as last-fact checked"
      (fact
        (swap! run-count inc)
        (+ ?a ?b) => ?c)
      ?a ?b ?c
      1  2  3
      2  2  4)
    
    (fact :check-only-at-load-time
      (without-changing-cumulative-totals (recheck-fact))
      @run-count => 4)

    ;; Facts mark themselves as last-fact-checked each time they're
    ;; rechecked.  
    (fact (+ 1 1) => 2)
    (def one-plus-one (last-fact-checked))
    (fact (+ 2 2) => 4)
    (def two-plus-two (last-fact-checked))

    (fact :check-only-at-load-time
      (without-changing-cumulative-totals (recheck-fact))
      (last-fact-checked) => (exactly two-plus-two)
      (check-one-fact one-plus-one)
      (last-fact-checked) => (exactly one-plus-one))
    )


;;;; ==== PART 5: Utilities

(fact "can find paths to load from project.clj"
  (fact "if it exists"
    (project-directories) => ["/test1" "/src1"]
    (provided (leiningen.core.project/read) => {:test-paths ["/test1"]
                                                :source-paths ["/src1"]}))

  (fact "and provides a default if it does not"
    (project-directories) => ["test"]
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

(fact "decomposing arglists"
  (against-background
    (compendium/all-facts<>) => ['ns.all-facts])

  ;; just namespaces
  (let [args '[ns.ns ns.ns2]]
    (deduce-user-intention args :for-in-memory-facts)
    => (contains {:all? false,:namespaces args}))

  ;; all and print level
  (let [args '[:all :print-nothing]]
    (deduce-user-intention args :for-in-memory-facts)
    => (contains {:all? true,
                  :original-args args
                  :print-level :print-nothing}))

  ;; all + redundant namespace, print level, filters
  ;; also check that the filter function is installed
  (let [args '[:all ns.ns :print-namespaces "name-match" :keyword]
        actual (deduce-user-intention args :for-in-memory-facts)]
    actual => (contains {:all? true,
                         :original-args args
                         :print-level :print-namespaces
                         :filters ["name-match" :keyword]})
    ( (:filter-function actual) (with-meta (fn[]) {:keyword true})) => true)

  ;; From disk, all, print level
  (let [args '[:all :print-namespaces]]
    (deduce-user-intention args :for-facts-anywhere)
    => (contains {:all? true,
                  :namespaces '[ns.project]
                  :original-args args
                  :filters nil})
    (provided (#'midje.repl/project-namespaces) => ['ns.project]))


  ;; From disk, partial namespace, default print level, filter
  (let [args '[midje.repl.* :integration]]
    (config/with-augmented-config {:print-level :print-nothing}
      (deduce-user-intention args :for-facts-anywhere))
    => (contains {:all? false,
                  :namespaces '[midje.repl.foo midje.repl.bar]
                  :original-args args
                  :print-level :print-nothing
                  :filters [:integration]})
      (provided (#'midje.repl/unglob-partial-namespaces ['midje.repl.*])
                => '[midje.repl.foo midje.repl.bar]))
  )

 (without-changing-cumulative-totals
  (fact "print-levels are not stored if not given"
    (deduce-user-intention '[:all :metadata] :for-in-memory-facts)
    => (contains {:fetch-default-args '[:all :metadata]})

    (deduce-user-intention '[midje.t-repl :metadata] :for-facts-anywhere)
    => (contains {:fetch-default-args '[midje.t-repl :metadata]
                  :load-default-args '[midje.t-repl :metadata]})))
              

)      ; confirming-cumulative-totals-not-stepped-on

)
