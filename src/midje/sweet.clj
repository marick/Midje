(require 'midje.Bootstrap)
(midje.Bootstrap/bootstrap)

(ns ^{:doc "A TDD library for Clojure that supports top-down ('mockish') TDD, 
            encourages readable tests, provides a smooth migration path from 
            clojure.test, balances abstraction and concreteness, and strives for 
            graciousness."}
  midje.sweet
  (:require midje.config) ; This should load first.
  (:use midje.clojure.core
        midje.production-mode
        midje.util.exceptions
        midje.util.debugging
        midje.parsing.util.core
        [midje.parsing.util.wrapping :only [put-wrappers-into-effect]]
        [midje.parsing.util.file-position :only [set-fallback-line-number-from positioned-form]]
        [midje.parsing.1-to-explicit-form.facts :only [complete-fact-transformation
                                  midjcoexpand unparse-edited-fact]] 
        [clojure.algo.monads :only [domonad]])
  (:require [midje.parsing.1-to-explicit-form.background :as background]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.1-to-explicit-form.future-facts :as parse-future-fact]
            [midje.parsing.1-to-explicit-form.metaconstants :as parse-metaconstants]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.0-to-fact-form.tabular :as tabular]
            [midje.parsing.0-to-fact-form.formulas :as parse-formulas]
            [midje.emission.api :as emit]
            [midje.doc :as doc]
            [midje.data.nested-facts :as nested-facts]
            midje.checkers))

(immigrate 'midje.checkers)
(immigrate 'midje.semi-sweet)

;; Following two are required because `intern` doesn't transfer "dynamicity".
(def ^{:doc "True by default.  If set to false, Midje checks are not
             included into production code, whether compiled or loaded."
       :dynamic true}
  *include-midje-checks* *include-midje-checks*)

;; TODO: How is this different from defalias?
(defn- intern+keep-meta [ns sym v]
  (intern ns sym v)
  (let [newguy (get (ns-interns ns) sym)]
    (alter-meta! newguy merge (meta v))))

(intern+keep-meta *ns* 'before  #'background/before)
(intern+keep-meta *ns* 'after   #'background/after)
(intern+keep-meta *ns* 'around  #'background/around)
(intern+keep-meta *ns* 'formula #'parse-formulas/formula)

(when (doc/appropriate?)
  (immigrate-from 'midje.doc doc/for-sweet)
  (doc/midje-notice))

(defmacro background
 " Puts a series of *background changers* into effect from the point of
   execution until the end of the file. They are also in effect when
   a loaded fact is rechecked (as with `midje.repl/recheck-fact`).

   See `(doc midje-background-changers)` for details on background
   changers. See `(guide background-prerequisites)` and `(guide
   setup-and-teardown)` for more.

   Examples:

      (background (f 33) => 12, (f 34) => 21)
      (background (before :facts (reset! some-atom 0)))"
 [& background-changers]
 (when (user-desires-checking?)
   (error/parse-and-catch-failure &form
    #(do                                   
       (background/assert-right-shape! &form)
       (put-wrappers-into-effect (background/background-wrappers
                                  (arglist-undoing-nesting background-changers)))))))

(defmacro against-background
 " Puts a series of *background changers* into effect until the end
   of the `against-background` scope. They remain in effect when a
   loaded fact is rechecked (as with `midje.repl/recheck-fact`).

   See `(doc midje-background-changers)` for details on background
   changers. See `(guide background-prerequisites)` and `(guide
   setup-and-teardown)` for more.

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
  (if (user-desires-checking?)
    (error/parse-and-catch-failure &form #(midjcoexpand &form))
    `(do ~@foreground-forms)))
    
(defmacro fact 
  "A fact is a statement about code:
  
  (fact \"one plus one is two\"
    (+ 1 1) => 2)
        
  You can make facts relative to other information:
  
  (defn g [x] nil)
  (defn f [x] (+ (g x) (g x)))
  
  (fact \"f of some arg, ..x.. calls g twice w/ the same arg,
          (..x..), that was sent to f, and adds up the results\"
    (f ..x..) => 12 
    (provided (g ..x..) => 6 :times 2))
    
  For more info, see on the wiki: 
  metaconstants, checkers, arrows and specifying call counts"
  [& _] ; we work off &form, not the arguments
  (when (user-desires-checking?)
    (error/parse-and-catch-failure &form
      #(do (set-fallback-line-number-from &form)
           (let [[metadata forms] (parse-metadata/separate-metadata &form)
                 [background remainder] (background/separate-background-forms forms)]
             (if (seq background)
               (positioned-form `(against-background [~@background]
                                   ~(unparse-edited-fact metadata remainder))
                                (:line (meta &form)))
               (complete-fact-transformation metadata remainder)))))))

(defmacro facts 
  "Alias for fact."
  [& forms]
  (when (user-desires-checking?)
    (with-meta `(fact ~@forms) (meta &form))))

(parse-future-fact/generate-variants)
(parse-formulas/generate-future-formula-variants)

(defmacro tabular 
  "Generate a table of related facts.
  
   Ex. (tabular \"table of simple math\" 
         (fact (+ a b) => c)
           
           a b      c
           1 2      3
           3 4      7
           9 10     19 )"
  {:arglists '([doc-string? fact table])}
  [& _]
  (set-fallback-line-number-from &form)
  (tabular/parse (keys &env) &form))


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
  (let [[metadata body] (parse-metadata/separate-multi-fact-metadata forms)]
    (parse-metadata/with-wrapped-metadata metadata 
      (midjcoexpand `(do ~@body)))))


