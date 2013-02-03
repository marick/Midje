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
        midje.error-handling.validation-errors
        midje.util.debugging
        [midje.parsing.util.wrapping :only [put-wrappers-into-effect]]
        [midje.parsing.util.file-position :only [set-fallback-line-number-from]]
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
  "Runs facts against setup code which is run before, around, or after 
   :contents, :facts or :checks. Optionally, contains one or more provided forms 
   that apply for all facts until the end of the file. 
   All effects of `background` last until the end of the file."
  [& state-descriptions]
  (when (user-desires-checking?)
    (background/when-valid &form
      (put-wrappers-into-effect (background/background-wrappers state-descriptions)))))

(defmacro against-background 
  "Runs facts against setup code which is run before, around, or after 
   :contents, :facts or :checks. Optionally, contains one or more provided forms 
   that apply for all facts run within the against-background form. 
   All effects of `against-background` last until the end of the scope it surrounds."
  [background-forms & foreground-forms]
  (if (user-desires-checking?)
      (midjcoexpand `(against-background ~background-forms ~@foreground-forms))
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
      #(domonad validate-m [_ (validate &form)
                            [metadata forms] (parse-metadata/separate-metadata &form)]
        (do 
          (set-fallback-line-number-from &form)
          (let [[background remainder] (background/separate-background-forms forms)]
            (if (seq background)
              `(against-background ~background
                 ~(unparse-edited-fact metadata remainder))
              (complete-fact-transformation metadata remainder))))))))

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


