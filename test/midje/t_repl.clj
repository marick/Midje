(ns midje.t-repl
  (:require [midje.repl :refer :all]
            [midje.test-util :refer :all]
            [midje.config :as config]
            [midje.emission.state :as state]
            [midje.data.compendium :as compendium]
            [midje.data.project-state :as project-state]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.scheduling :as scheduling]
            midje.util))

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
        (project-state/namespaces) => ['midje.t-repl-helper])
      (map fact-name (fetch-facts :all)) => ["a simple test"])

    (fact "no arguments repeats previous arguments"
      (load-facts 'midje.t-repl-helper :non-featherian :print-no-summary)
      (forget-facts :all)
      (load-facts)
      (map fact-name (fetch-facts)) => ["a non-featherian test"])

    (fact "repetition not affected by intervening check-facts"
      (load-facts 'midje.t-repl-helper :non-featherian :print-no-summary)
      (check-facts ':all :print-no-summary)
      (forget-facts :all)
      (load-facts)
      (map fact-name (fetch-facts :all)) => ["a non-featherian test"])

    (fact "load-facts sets up default arguments for fetch-facts"
      (load-facts :all "simple" :print-no-summary) => anything
      (provided
        (project-state/namespaces) => ['midje.t-repl-helper])

      (defaulting-args [] :memory-command)
      => (contains '{:given-level-args [:print-no-summary]
                     :given-filter-args ["simple"]
                     :given-namespace-args [:all]})

      (load-facts 'midje.t-repl-h* :non-featherian :print-no-summary) => anything
      (provided
        (project-state/unglob-partial-namespaces ['midje.t-repl-h*]) => '[midje.t-repl-helper])

      (defaulting-args [] :memory-command)
      => (contains '{:given-level-args [:print-no-summary]
                    :given-filter-args [:non-featherian]
                    :given-namespace-args [midje.t-repl-helper]}))

  )
  )

;;;; ==== PART 2: Working with loaded facts


                                ;;; Fetching facts
 (compendium/fresh!)

;;; Note that facts must be returned in alphetical order by namespace,
;;; then by creation order within namespace.
 (add-fact :midje/namespace 'midje.parsing.1-to-explicit-form.facts :midje/name "f - second - midje.parsing.1-to-explicit-form.facts")
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

   (let [expected ["f - second - midje.parsing.1-to-explicit-form.facts"]]
     (names (fetch-facts 'midje.parsing.1-to-explicit-form.facts)) => expected
     (names (fetch-facts)) => expected)

   ;; if explicit namespace order is given, it's obeyed
   (let [expected ["m - fourth - midje.t-repl" "a - fifth - midje.t-repl" "z - first - integration" ]]
     (names (fetch-facts 'midje.t-repl 'midje.config)) => expected
     (names (fetch-facts)) => expected)

   (let [expected ["z - first - integration" "f - second - midje.parsing.1-to-explicit-form.facts" "p - third - midje.repl" "m - fourth - midje.t-repl" "a - fifth - midje.t-repl"]]
     (names (fetch-facts :all)) => expected
     (names (fetch-facts)) => expected)

   (let [expected ["z - first - integration" "f - second - midje.parsing.1-to-explicit-form.facts"]]
     (names (fetch-facts :all "n")) => expected
     (names (fetch-facts)) => expected)

   (names (fetch-facts 'midje.parsing.1-to-explicit-form.facts #"q")) => []
   (names (fetch-facts :all :integration)) => ["z - first - integration"]
   (names (fetch-facts *ns* :integration)) => []
   (names (fetch-facts :all #(= (:midje/name %) "p - third - midje.repl"))) => ["p - third - midje.repl"]
   (names (fetch-facts "midje.parsing.1-to-explicit-form.facts" "p - third - midje.repl" :all))
   => ["f - second - midje.parsing.1-to-explicit-form.facts" "p - third - midje.repl"]
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
   (add-fact :midje/name "bcdef" :midje/namespace 'midje.doc)
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
      (add-fact :midje/namespace 'midje.parsing.1-to-explicit-form.facts :midje/name "not forgotten")
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
      (add-fact :midje/namespace 'midje.parsing.1-to-explicit-form.facts)
      (forget-facts 'midje.parsing.1-to-explicit-form.facts)
      (map fact-name (fetch-facts)) => ["a non-featherian test"]))
  )




                                ;;; Checking facts

 (forget-facts :all)
 (fact "the-name" 1 => 1)
 (def the-fact (first (fetch-facts 'midje.t-repl)))

         ;;; Checking one fact

 (without-changing-cumulative-totals
  (let [check-result (check-one-fact the-fact)]
    (fact :check-only-at-load-time
      (:midje-passes (state/output-counters)) => 1
      check-result => true)))

 ;; Obeying metadata filters
 (let [string-result ; will only be visible for facts defined at the repl.
       (config/with-augmented-config {:fact-filter (with-meta (constantly false)
                                                     {:created-from ["desiderata"]})}

         (check-one-fact the-fact))]
   (fact :check-only-at-load-time
     string-result => #"ignored because of the current configuration"
     string-result => #"matching \[\"desiderata\"\]"))



        ;;; Checking multiple facts

;; Create (silently, without affecting totals) a fact to be rechecked later.
(silent-fact "error fact" 1 => 2)

 (without-changing-cumulative-totals
  (fact "you can give print-level arguments."
    :check-only-at-load-time
    (config/with-augmented-config {:print-level :print-nothing}
      ;; Despite above config.
      (captured-output (check-facts 'midje.t-repl :print-namespaces)) => #"Namespace midje.t-repl")

    (fact "return values still hold, though"
      (check-facts 'midje.t-repl "the-name" :print-nothing) => true
      (check-facts 'midje.t-repl :print-nothing) => false))

  (fact "check-facts resets the counter"
    :check-only-at-load-time
    (check-facts 'midje.t-repl "the-name" :print-nothing)
    (:midje-passes (state/output-counters)) => 1
    ;; no change below because of auto-zeroing
    (check-facts 'midje.t-repl "the-name" :print-nothing)
    (:midje-passes (state/output-counters)) => 1)

  (fact "check-facts takes the usual arguments"
    :check-only-at-load-time
    ;; Most of the work is done by fetch-facts, but let's try a few
    (config/with-augmented-config {:print-level :print-nothing}
      (check-facts 'midje.config)
      (:midje-passes (state/output-counters)) => 0
      (:midje-failures (state/output-counters)) => 0

      (check-facts *ns*)
      (:midje-passes (state/output-counters)) => 1
      (:midje-failures (state/output-counters)) => 1

      (check-facts 'midje.t-repl "error fact")
      (:midje-passes (state/output-counters)) => 0
      (:midje-failures (state/output-counters)) => 1

      (check-facts :all :tinkerbell)
      (:midje-passes (state/output-counters)) => 0
      (:midje-failures (state/output-counters)) => 0))

  (fact "check-facts affects the next check-facts/fetch-facts"
    (check-facts 'midje.t-repl "the-name" :print-nothing) => true
    (check-facts) => true
    (map fact-name (fetch-facts)) => ["the-name"])

  ;; Old bug. A failure prevented further facts from being checked.
  (forget-facts :all)
  (silent-fact "1" (+ 1 2) => 3333)  ; establish the failing fact
  (fact "2" (+ 1 2) => 3)            ; follow it with another

  (fact "confirm the ordering of the two facts"
    :check-only-at-load-time
    (map fact-name (fetch-facts 'midje.t-repl)) => ["1" "2"])

  (fact "Even though the first fact failed, both facts were checked"
    :check-only-at-load-time
    (check-facts 'midje.t-repl :print-nothing)
    (state/output-counters:midje-passes) => 1
    (state/output-counters:midje-failures) => 1)
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
      (:midje-passes (state/output-counters)) => 1
      (:midje-failures (state/output-counters)) => 0)

    (fact "It also prints a summary"
      :check-only-at-load-time
      (config/with-augmented-config {:print-level :print-normally}
        (captured-output (recheck-fact)) => #"All checks \(1\) succeeded"))

    (fact "It can take a print-level argument"
      :check-only-at-load-time
      (captured-output (recheck-fact :print-namespaces)) => #"midje.t-repl")
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

;;;; ==== PART 5: Autotest

(fact "autotest is driven by options"
  (set (keys (autotest-options))) => (contains #{:interval :files}))

(fact "options can be set"
  (let [old-interval (:interval (autotest-options))]
    (:interval (autotest-options)) =not=> 832
    (set-autotest-option! :interval 832)
    (:interval (autotest-options)) => 832
    (set-autotest-option! :interval old-interval)))

(fact "autotest"
  (against-background (autotest-options) => ..options..
                      ..options.. =contains=> {:interval ..interval..})
  (fact "schedules reactions to changes"
    (autotest) => anything
    (provided
      (project-state/load-everything ..options..) => anything
      (scheduling/schedule :autotest
                           (project-state/mkfn:react-to-changes ..options..)
                           ..interval..) => anything))

  (fact "can stop autotesting"
    (autotest :stop) => anything
    (provided
      (scheduling/stop :autotest) => anything))

  (fact "can pause"
    ;; This is the same as stopping, except for the expectation that the
    ;; next call will be (autotest :resume) rather than ().
    (autotest :pause) => anything
    (provided
      (scheduling/stop :autotest) => anything))

  (fact "can resume"
    (captured-output (autotest :resume)) => anything
    (provided
      (project-state/load-everything) => anything :times 0 ;; NOT called
      (scheduling/schedule :autotest
                           (project-state/mkfn:react-to-changes ..options..)
                           ..interval..)
      => anything))

  (fact "can be used to set default values"
    (config/with-augmented-config {:partial-prerequisites true}
      (autotest :files "src" "test" :interval 5) => anything
      (provided
        (set-autotest-option! :files ["src" "test"]) => anything
        (set-autotest-option! :interval 5) => anything
        (autotest) => anything)))

  (fact "Can reset directories"
    (config/with-augmented-config {:partial-prerequisites true}
      (autotest :all) => irrelevant
      (provided
        (set-autotest-option! :files (autotest-default-dirs)) => irrelevant
        (autotest) => anything)))

  (fact "dirs is just an alias for files"
    (config/with-augmented-config {:partial-prerequisites true}
      (autotest :dirs "src" "test") => anything
      (provided
        (set-autotest-option! :files ["src" "test"]) => anything
        (autotest) => anything)))

  (fact "skips adding nonexistent files or dirs"
    (captured-output (autotest :files "blah/src" "blah/test")) => anything
    (captured-output (autotest :files "blah/src.clj" "blah/test.clj")) => anything

    (captured-output (autotest :dirs "blah/src" "blah/test")) => anything
    (captured-output (autotest :dirs "blah/src.clj" "blah/test.clj")) => anything

    (provided
      (set-autotest-option! :files anything) => irrelevant :times 0))
)





;;;; ==== PART 6: Utilities

(fact "decomposing arglists"
  (against-background
    (compendium/all-facts<>) => ['ns.all-facts])

  ;; just namespaces
  (let [args '[ns.ns ns.ns2]]
    (deduce-user-intention args :memory-command)
    => (contains {:all? false
                  :namespaces-to-use args
                  :memory-command args}))

  ;; all and print level
  (let [args '[:all :print-nothing]]
    (deduce-user-intention args :memory-command)
    => (contains {:all? true,
                  :print-level :print-nothing
                  :memory-command [:all]
                  :namespaces-to-use [:all]
                  :given-namespace-args [:all]
                  :given-filter-args empty?
                  :given-level-args [:print-nothing]}))

  ;; all + redundant namespace, print level, filters
  ;; also check that the filter function is installed
  (let [args '[:all ns.ns :print-namespaces "name-match" :keyword]
        actual (deduce-user-intention args :memory-command)]
    actual => (contains {:all? true,
                         :print-level :print-namespaces
                         :namespaces-to-use '[:all ns.ns]
                         :memory-command '[:all ns.ns]
                         :given-namespace-args '[:all ns.ns]
                         :given-filter-args ["name-match" :keyword]
                         :given-level-args [:print-namespaces]})
    ( (:filter-function actual) (with-meta (fn[]) {:keyword true})) => true)

  ;; From disk, all, print level
  (let [args '[:all :print-namespaces]]
    (deduce-user-intention args :disk-command)
    => (contains {:all? true,
                  :print-level :print-namespaces
                  :namespaces-to-use '[midje.repl.foo midje.repl.bar]
                  :memory-command [:all]
                  :disk-command [:all]
                  :given-namespace-args [:all]
                  :given-filter-args empty?
                  :given-level-args [:print-namespaces]})
    (provided (project-state/namespaces)
                => '[midje.repl.foo midje.repl.bar]))

  ;; From disk, partial namespace, default print level, filter
  (let [args '[midje.repl.* :integration]]
    (config/with-augmented-config {:print-level :print-nothing}
      (deduce-user-intention args :disk-command))
    => (contains {:all? false,
                  :print-level :print-nothing
                  :memory-command '[midje.repl.foo midje.repl.bar]
                  :disk-command '[midje.repl.*]
                  :given-namespace-args '[midje.repl.*]
                  :given-filter-args [:integration]
                  :given-level-args empty?
                  :namespaces-to-use '[midje.repl.foo midje.repl.bar]})
    (provided (project-state/unglob-partial-namespaces ['midje.repl.*])
              => '[midje.repl.foo midje.repl.bar]))
  )

(facts "about what it means to leave out arguments"
  ;; Command type doesn't matter for this test. I'll vary it just because.
  (letfn [(set-previous-args [args command-type]
            (and-update-defaults! (deduce-user-intention args command-type)
                                  command-type))
          (after [command-type first-args second-args]
            (set-previous-args first-args command-type)
            (deduce-user-intention second-args command-type))]

    (fact "as before, all left out means all are replaced"
      (after :memory-command '[ns :filter :print-facts] [])
      => (contains {:given-namespace-args '[ns]
                    :given-filter-args '[:filter]
                    :given-level-args '[:print-facts]}))

    (fact "filter can be replaced"
      (after :memory-command '[ns :filter :print-facts]
                              [   "other filter"])
      => (contains {:given-namespace-args '[ns]
                    :given-filter-args '["other filter"]
                    :given-level-args '[:print-facts]}))

    (fact "print-level can be replaced, too"
      (after :disk-command '[ns :filter        :print-facts]
                            [                  :print-nothing])
      => (contains {:given-namespace-args '[ns]
                    :given-filter-args '[:filter]
                    :given-level-args '[:print-nothing]}))

    (fact "both non-namespace args can be replaced"
      (after :memory-command '[ns :filter        :print-facts]
                              [   "other filter" :print-nothing])
      => (contains {:given-namespace-args '[ns]
                    :given-filter-args '["other filter"]
                    :given-level-args '[:print-nothing]}))

    (fact ":all counts as a namespace replacement"
      (after :memory-command '[ns :filter :print-facts] [:all])
      => (contains {:given-namespace-args '[:all]
                    :given-filter-args empty?
                    :given-level-args empty?}))


    (fact "mentioning levels in replacement doesn't stop filters from being zeroed"
      (after :memory-command '[:all :filter :print-facts] '[ns :print-nothing])
      => (contains {:given-namespace-args '[ns]
                    :given-filter-args empty?
                    :given-level-args '[:print-nothing]}))))



(without-changing-cumulative-totals
  (fact "print-levels are not recorded if not given"
    (and-update-defaults!
      (deduce-user-intention '[:all :metadata] :memory-command)
      :memory-command)
    (deduce-user-intention [] :memory-command)
    => (contains {:given-level-args []})


    (and-update-defaults!
      (deduce-user-intention '[midje.t-repl :metadata] :disk-command)
      :disk-command)

    (deduce-user-intention [] :memory-command) => (contains {:given-level-args []})
    (deduce-user-intention [] :disk-command) => (contains {:given-level-args []})))


(without-changing-cumulative-totals
 (forget-facts :all)
 (config/with-augmented-config {:print-level :print-nothing}
   (fact :fail 1 => 2)
   (fact :succeed 1 => 1)

   (let [mixed-results (check-facts-once-given (fetch-facts :all))
         all-pass (check-facts-once-given (fetch-facts :succeed))
         all-fail (check-facts-once-given (fetch-facts :fail))
         no-results (check-facts-once-given (fetch-facts :none))]
     (config/with-augmented-config {:print-level :print-no-summary}
       (fact
         mixed-results => false
         all-pass => true
         all-fail => false
         no-results => nil)))))





)      ; confirming-cumulative-totals-not-stepped-on


