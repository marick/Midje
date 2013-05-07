(require 'midje.Bootstrap)
(midje.Bootstrap/bootstrap)

(ns ^{:doc "A TDD library for Clojure that supports top-down ('mockish') TDD, 
            encourages readable tests, provides a smooth migration path from 
            clojure.test, balances abstraction and concreteness, and strives for 
            graciousness."}
  midje.sweet
  (:require midje.config) ; This should load first.
  ;; The following lets us avoid a circular dependency. Sigh.
  (:require midje.parsing.1-to-explicit-form.future-facts)

  (:use midje.clojure.core
        midje.parsing.util.core
        midje.production-mode)
  ;; For immigration
  (:require midje.semi-sweet
            [midje.doc :as doc]
            midje.checkers)
  (:require [clojure.string :as str]
            [midje.util.pile :as pile]
            [midje.util.exceptions :as exceptions]
            [midje.util.ecosystem :as ecosystem]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.future-variants :as future-variants]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.wrapping :as wrapping]
            [midje.parsing.util.file-position :as position]
            [midje.parsing.0-to-fact-form.tabular :as parse-tabular]
            [midje.parsing.0-to-fact-form.formulas :as parse-formulas]
            [midje.parsing.1-to-explicit-form.facts :as parse-facts]
            [midje.parsing.1-to-explicit-form.background :as background]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.1-to-explicit-form.metaconstants :as parse-metaconstants]
            [midje.data.nested-facts :as nested-facts]
            [midje.emission.api :as emit]))

(immigrate 'midje.checkers)
(immigrate 'midje.semi-sweet)

(def include-midje-checks
  "True by default. If set to false, Midje checks are not
   included into production code, whether compiled or loaded.

   Note that the variable must be set, as with `alter-var-root`
   or `def`, not bound, as with `binding`."
  true)

(defonce
  ^{:doc "This variable is defunct. Use `include-midje-checks` instead."
     :dynamic true}
  *include-midje-checks* :original-truthy-value)

(set-validator! #'*include-midje-checks*
                (fn [val]
                  (when-not (= val :original-truthy-value)
                    (emit/fail {:type :parse-error
                                :notes ["*include-midje-checks* is defunct. Use `include-midje-checks` instead."]
                                :position (position/compile-time-fallback-position)}))
                  true))

(defn- unfinished* [names]
  (pile/macro-for [name names]
     `(do
        (defn ~name [& args#]
          (let [pprint# (partial cl-format nil "~S")]
            (throw (exceptions/user-error (format "#'%s has no implementation, but it was called like this:%s(%s %s)" 
                                                  '~name ecosystem/line-separator '~name
                                                  (str/join " " (map pprint# args#)))))))
        
        ;; A reliable way of determining if an `unfinished` function has since been defined.
        (alter-meta! (var ~name) assoc :midje/unfinished-fun ~name)
        :ok)))

(defmacro unfinished
    "Defines a list of names as functions that have no implementation yet. They will
     throw Errors if ever called."
    [& names] (unfinished* names))
  
(defalias before  background/before)
(defalias after   background/after)
(defalias around  background/around)
(defalias formula parse-formulas/formula)

(when (doc/appropriate?)
  (immigrate-from 'midje.doc doc/for-sweet)
  (doc/midje-notice))

(defmacro background
 " Puts a series of *background changers* into effect from the point of
   execution until the end of the file. They are also in effect when
   a loaded fact is rechecked (as with `midje.repl/recheck-fact`).

   See `(doc midje-background-changers)` for details on background
   changers.

   Examples:

      (background (f 33) => 12, (f 34) => 21)
      (background (before :facts (reset! some-atom 0)))
 "
 [& background-changers]
 (when (user-desires-checking?)
   (error/parse-and-catch-failure &form
    #(do                                   
       (background/assert-right-shape! &form)
       (wrapping/put-wrappers-into-effect (background/background-wrappers
                                           (arglist-undoing-nesting background-changers)))))))

(defmacro against-background
 " Puts a series of *background changers* into effect until the end
   of the `against-background` scope. They remain in effect when a
   loaded fact is rechecked (as with `midje.repl/recheck-fact`).

   See `(doc midje-background-changers)` for details on background
   changers.

   `against-background` can be used in two ways. In the first, it has
   a `let`-like syntax that wraps a series of facts:

      (against-background [(f 33) => 12
                           (f 34) => 21
                           (before :facts (reset! some-atom 0))]
        (fact...)
        (fact...))

   In the second, it can be placed as either the first or last form in
   a fact, in which case it is taken to \"wrap\" the entire fact:

     (against-background (f 1) => :default, (g 1) => :default)

   Note that in this case the square brackets can be omitted."
  [background-forms & foreground-forms]
  (when (user-desires-checking?)
    (error/parse-and-catch-failure &form #(parse-facts/midjcoexpand &form))))

(defmacro with-state-changes
  "Describe how state should change before or after enclosed facts. Example:

       (with-state-changes [(before :facts (reset! state 0))
                            (after :facts (reset! state 1111))]
         (fact ...)
         (fact ...))"
  [& forms]
  (position/positioned-form `(midje.sweet/against-background ~@forms) &form))


(defmacro namespace-state-changes
  "Applies arguments to any facts later created in the namespace. Often used
   in the repl. Example:

       (namespace-state-changes (before :facts (reset! state 0))
                                (after :facts (reset! state 1111)))

   A later use of `namespace-state-changes` replaces the effect of an earlier
   one. To \"turn off\" a previous use, do this:

       (namespace-state-changes)
  "
  [& instructions]
  (position/positioned-form `(midje.sweet/background ~@instructions) &form))

(defmacro fact 
  "A fact is a statement about code:
  
  (fact \"one plus one is two\"
    (+ 1 1) => 2)
        
  Facts may describe one functions dependency on another:
  
  (fact 
    (f ..x..) => 12 
    (provided (g ..x..) => 6))
  "
  [& _] ; we work off &form, not the arguments
  (when (user-desires-checking?)
    (error/parse-and-catch-failure &form
      #(do (position/set-fallback-line-number-from &form)
           (let [[metadata forms] (parse-metadata/separate-metadata &form)
                 [background remainder] (background/separate-background-forms forms)]
             (if (seq background)
               (position/positioned-form `(against-background [~@background]
                                            ~(parse-facts/unparse-edited-fact metadata remainder))
                                         &form)
               (parse-facts/complete-fact-transformation metadata remainder)))))))

(defmacro facts 
  "Alias for fact."
  [& forms]
  (when (user-desires-checking?)
    (with-meta `(fact ~@forms) (meta &form))))

(future-variants/generate-future-fact-variants)
(future-variants/generate-future-formula-variants)

(defmacro tabular 
  "Generate a table of related facts.
  
   Ex. (tabular \"table of simple math\" 
         (fact (+ ?a ?b) => ?c)
           
           ?a ?b      ?c
            1  2       3
            3  4       7
            9 10      19 )"
  {:arglists '([doc-string? fact table])}
  [& _]
  (position/set-fallback-line-number-from &form)
  (parse-tabular/parse (keys &env) &form))


(defmacro metaconstants
  "For a few operations, such as printing and equality checking,
   the Clojure AOT-compiler becomes confused by Midje's auto-generation
   of metaconstants. If AOT-compiled tests fail when on-the-fly
   compiled tests failed, declare your metaconstants before use.
   Example:
     (metaconstants ..m.. ..m.... .mc.)"
  [& symbols]
  `(parse-metaconstants/predefine-metaconstants-from-form '~symbols))

(defmacro fact-group
  "Supply default metadata to all facts in the body."
  [& forms]
  (when (user-desires-checking?)
    (let [[metadata body] (parse-metadata/separate-multi-fact-metadata forms)]
      (parse-metadata/with-wrapped-metadata metadata 
        (parse-facts/midjcoexpand `(do ~@body))))))


