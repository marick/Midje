(ns ^{:doc "In-repl user documentation"}
  midje.doc
  (:require [clojure.java.browse :as browse]
            [midje.config :as config]
            [midje.emission.colorize :as color]
            [midje.util.ecosystem :as ecosystem]))

(def appropriate? config/running-in-repl?)

(defn repl-notice []
  (println (color/note "Run `(doc midje-repl)` for descriptions of Midje repl functions.")))

(defn midje-notice []
  (println (color/note "Run `(doc midje)` for Midje usage.")))

;;; KLUDGE: The list of values to be imported must be known to the importing namespaces.
;;; Don't forget to update them if adding new doc strings.

(def ^{:doc "
   Detailed help:
   (doc midje-facts)             -- The basic form.
   (doc midje-checkers)          -- Predefined predicates to use with arrows.
   (doc midje-arrows)            -- Alternate ways of describing the relationship
                                 -- between actual and expected values.

   (doc midje-prerequisites)     -- For top-down test-driven design.
   (doc midje-defining-checkers) -- Defining checkers
   (doc midje-setup)             -- Setup and teardown for facts and checkers.
   (doc midje-configuration)     -- Changing Midje's defaults.
   (doc midje-print-levels)      -- Changing Midje's verbosity.

   See also the `(guide)` macro, which takes you to pages in the
   user's guide.
   "} midje)


;; midje-repl
(def ^{:doc
  "
  Here are Midje repl functions. Use `doc` for more info on each one.
  To control verbosity of output, use the print levels described by
  `(doc midje-print-levels)`.
  Some functions take filter arguments that narrow down which facts
  within a namespace are acted upon. See the individual doc strings
  for details.

  ----- Autotest

  (autotest)       ; Reload changed files and all their dependents.

  ----- Loading facts
  You load facts by namespace.
  (load-facts <ns> <ns>...)
  (load-facts 'midje.util.*)      ; Load all namespaces below midje.util.
  (load-facts <ns> :integration)  ; Filter to just integration tests.
  (load-facts)                    ; Repeat most recent load-facts.

  ----- Checking facts, once loaded
  (check-facts <ns> <ns>...)        ; in given namespaces
  (check-facts :all)                ; all defined facts
  (check-facts :all :integration)   ; all integration tests

  Note: `check-facts` with no argument will check the same facts
  as the most recent `check-facts` or `load-facts`.

  ----- Rerunning facts
  (recheck-fact)                ; Check just-checked fact again.
  (recheck-fact :print-nothing) ; Check silently (but produce true/false).
  (rcf)                         ; Synonym for above.

  Note: facts with `:check-only-at-load-time` metadata do not get
  stored for rechecking.

  ----- Forgetting facts
  Same notation as the `check-facts` family, but with
  \"forget\" instead of \"check\".

  ----- Fetching facts
  Same notation as the `check-facts` family, but with
  \"fetch\" instead of \"check\".
  To check the returned facts, use `check-one-fact`:
     (map check-one-fact (fetch-facts :all))

  In addition, you can fetch the last fact checked with
  `(last-fact-checked)`. `(source-of-last-fact-checked)`
  gives you its source.

  To query fact metadata more easily, use these:
  -- (fact-name <ff>)         ; result might be nil
  -- (fact-source <ff>)
  -- (fact-file <ff>)
  -- (fact-line <ff>)
  -- (fact-namespace <ff>)    ; a symbol
  -- (fact-description <ff>)  ; the doc string; might be nil
  "} midje-repl)


;; print-levels
(def ^{:doc "
  The `load-facts`, `check-facts`, and `recheck-fact`
  functions normally print any fact failures and a final
  summary. The detail printed can be adjusted by passing
  either certain keywords or corresponding numbers to the
  functions. (The numbers are easier to remember.)
  For example, here's how you check all facts in the most
  verbose way:
    (check-facts :all 2)
    (check-facts :all :print-facts)

  Here are all the variants:

  :print-normally     (0)  -- failures and a summary.
  :print-no-summary  (-1)  -- failures only.
  :print-nothing     (-2)  -- nothing is printed.
                           -- (but return value can be checked)
  :print-namespaces   (1)  -- print the namespace for each group of facts.
  :print-facts        (2)  -- print fact descriptions in addition to namespaces.
"} midje-print-levels)

(defn alternate-doc-source [copy original]
  (intern *ns* (vary-meta copy assoc :doc (:doc (meta (resolve original))))))

(alternate-doc-source 'midje-print-level 'midje-print-levels)


(def ^{:doc "
  * A common form:
    (fact \"fact name / doc string\"
      (let [result (prime-ish 5)]
        result => odd?
        result => (roughly 13)))

  * Nested facts
    (facts \"about life\"
      (facts \"about birth\"...)
      (facts \"about childhood\"...)
      ...)

  * Tabular facts
    (tabular
      (fact (+ ?x ?y) => 3)
      ?x   ?y
       1    2
       0    3)

  * Metadata
    (fact :integration ...)
    (fact {:priority 5} ...)
"} midje-facts)

(alternate-doc-source 'midje-fact 'midje-facts)

(def ^{:doc "
  (facts \"about checkers\"
    (f) => truthy
    (f) => falsey
    (f) => irrelevant ; or `anything`
    (f) => (exactly odd?) ; when you expect a particular function
    (f) => (roughly 10 0.1)
    (f) => (throws SomeException #\"with message\")
    (f) => (contains [1 2 3]) ; works with strings, maps, etc.
    (f) => (contains [1 2 3] :in-any-order :gaps-ok)
    (f) => (just [1 2 3])
    (f) => (has every? odd?)
    (f) => (nine-of odd?) ; must be exactly 9 odd values.
    (f) => (every-checker odd? (roughly 9)) ; both must be true
    (f) => (some-checker odd? (roughly 9))) ; one must be true
  "} midje-checkers)

(def ^{:doc "
  Any function can be used on the right-hand side of a check:
      (fact 500 => (fn [actual] (> actual 100)))

  In the left-hand side of a prerequisite, use `checker` or
  `as-checker` to define the checker:
      (provided
         (f (checker [arg] (> arg 100))) => 3
         (h (as-checker even?)) => 4)

  Checkers can be named with `defchecker`:
        (defchecker twosie [actual]
           (and (pos? actual) (even? actual)))
        (fact 2 => twosie)

        (defchecker roughly [expected delta]
          (checker [actual]
             (and (number? actual)
                  ...)))
        (fact 1.1 => (roughly 1 0.2))

  Chatty checkers print the values of subexpressions in the
  case of a failure:

       (fact 4 => (chatty-checker [actual] (< (h actual) (g actual))))

  Use `every-checker` or `some-checker` to define checkers for
  boolean expressions:

       (def pleasingly-positive (every-checker number? even? pos?))
       (fact 4 => pleasingly-positive)

       (def unnatural-number (some-checker (complement number?) odd? neg?))
       (fact 'fred => unnatural-number)
  "} midje-defining-checkers)

(def ^{:doc "
  * Prerequisites and top-down TDD:
    (unfinished char-value chars)
    (fact \"a row value is composed of character values\"
       (row-value ..row..) => \"53\"
       (provided
         (chars ..row..) => [..five.. ..three..]
         (char-value ..five..) => \"5\"
         (char-value ..three..) => \"3\"))

  * Prerequisites can be defaulted for claims within a fact:
    (fact \"No one is ready until everyone is ready.\"
      (prerequisites (pilot-ready?) => true,
                     (copilot-ready?) => true,
                     (flight-engineer-ready?) => true)
      (ready?) => truthy
      (ready?) => falsey (provided (pilot-ready?) => false)
      (ready?) => falsey (provided (copilot-ready?) => false)
      (ready?) => falsey (provided (flight-engineer-ready?) => false))

  * Prerequisites apply to nested facts
    (facts \"about airplanes\"
       (prerequisites (wings) => 2
                      (engines => 2)
          (fact
             (prerequisite (crew) => 8)
             ...)))
"} midje-prerequisites)


(def ^{:doc
       "
  Setup and Teardown
  * Applies to all enclosed facts
    (with-state-changes [(before :facts (do-this))
                         (after :facts (do-that))]
       (fact ...)
       (fact ...))

  * For the rest of this namespace:

       (namespace-state-changes (before :facts (do-this))
                                (after :facts (do-that)))
"} midje-setup)

(alternate-doc-source 'midje-teardown 'midje-setup)

(def ^{:doc "
  * For checks
    5     =not=>    even?      ; Invert the check. Synonym: =deny=>
    (f)   =future=> halts      ; don't check, but issue reminder.
    (m x) =expands-to=> form   ; expand macro and check result

  * In prerequisites
    ..meta.. =contains=> {:a 1} ; partial specification of map-like object
    (f)      =streams=>  (range 5 1000)
    (f)      =throws=>   (IllegalArgumentException. \"boom\")
"} midje-arrows)

(def ^{:doc "
  On startup, Midje loads ${HOME}/.midje.clj and ./.midje.clj
  (in that order). To affect the default configuration, use
  code like this in those files:
      (change-defaults :visible-deprecation false
                       :visible-future false
                       :print-level :print-namespaces)

  If you want different configurations for repl and command-line
  Midje, use the `running-in-repl?` predicate.

  You can temporarily override the configuration like this:
      (midje.config/with-augmented-config {:print-level :print-no-summary}
         <forms>...)

  ------ Configuration keywords
  :print-level                  ; Verbosity of printing.
                                ; See `(doc midje-print-levels)`.

  :pretty-print                 ; Use formatting and color in test output
                                ; Default: true

  :visible-deprecation          ; Whether information about deprecated
                                ; features or functions is printed.
                                ; Default: true.

  :visible-future               ; Whether future facts produce output.
                                ; Default: true.
                                ; More: `(guide future-facts)`

  :visible-failure-namespace    ; Should failure messages include the
                                ; namespace name as well as the file name?
                                ; Default: false.

  :fact-filter                  ; A function applied to the metadata of
                                ; a fact to see if it should be checked.
                                ; Default: (constantly true)

  :check-after-creation         ; Should facts be checked as they're loaded?
                                ; Default: true.

  :emitter                      ; Namespace or pathname that contains
                                  an \"emitter\" of custom output.

  :partial-prerequisites        ; Whether the real function can be used.
                                ; Default false.
                                ; More: `(guide partial-prerequisites)`
"} midje-configuration)


(def ^{:doc "
   Background-changers are discouraged in favor of `with-state-changes`
   and `prerequisites`, but they still may sometimes be needed.

   There are two types of background-changers. The first provides a default
   prerequisite and has the same format as `provided`. Here's an example of
   two inside in a `background` form:

      (background (f 33) => 12, (f 34) => 21)

   The other form runs arbitrary code before, after, or around a fact or
   individual check. Here's how you execute some setup code before each of a
   series of facts:

      (against-background [(before :facts (reset! some-atom 0))]
         (fact ...)
         (fact ...))

   Here's how you execute some code after each check in a fact:

      (against-background [(after :checks (reset! some-atom 0))]
        (fact
          (f 1) => 2   ; Reset happens after.
          (f 2) => 3)) ; Reset happens after.

   You can execute code both before and after a fact like this:

      (against-background [(before :facts (reset! in-atom 0)
                                   :after (reset! out-atom 0))]
         (fact ...)
         (fact ...))

   You can also wrap code around a fact or check:

      (against-background [(around :facts (sql/with-connection db ?form))]
         ...)

   The two types of background-changers can be intermingled.
"} midje-background-changers)


(def guide-raw
 ['user-guide "https://github.com/marick/Midje/wiki"
  'concept-index "https://github.com/marick/Midje/wiki/Concept-index"
  'syntax-reference "https://github.com/marick/Midje/wiki/Syntax-and-a-little-semantics"
  '--- ""
  'file-an-issue ecosystem/issues-url
  'unfixed-syntax-errors ecosystem/syntax-errors-that-will-not-be-fixed
  '--- ""
  'future-facts  "https://github.com/marick/Midje/wiki/Future-facts"
  'tabular-facts "https://github.com/marick/Midje/wiki/Tabular-facts"
  'fact-metadata "https://github.com/marick/Midje/wiki/Using-metadata-to-filter-facts"
  "" ""
  'setup-and-teardown "https://github.com/marick/Midje/wiki/Setup-and-teardown"
  'prerequisites "https://github.com/marick/Midje/wiki/Describing-one-checkable%27s-prerequisites"
  'fact-wide-prerequisites "https://github.com/marick/Midje/wiki/Establishing-fact-wide-prerequisites"
  'partial-prerequisites "https://github.com/marick/Midje/wiki/Partial-prerequisites"
  "" ""
  'checking-sequences "https://github.com/marick/Midje/wiki/Checking-sequential-collections"
  'checking-maps-and-records "https://github.com/marick/Midje/wiki/Checking-maps-and-records"
  'checking-sets "https://github.com/marick/Midje/wiki/Checking-sets"
  'combining-checkers "https://github.com/marick/Midje/wiki/Combining-checkers"
  'chatty-checkers "https://github.com/marick/Midje/wiki/Chatty-checkers"

  ])

(def guide-topics (map first (partition 2 guide-raw)))
(def guide-urls (map second (partition 2 guide-raw)))

(def guide-map (zipmap guide-topics guide-urls))

(defmacro guide
  "Open a web page that addresses a particular topic"
  ([topic]
     `(if-let [url# (guide-map '~topic)]
        (browse/browse-url url#)
        (do
          (println "There is no such topic. Did you mean one of these?")
          (doseq [topic# guide-topics]
            (println "   " topic#)))))
  ([]
     `(guide :no-such-topic)))
