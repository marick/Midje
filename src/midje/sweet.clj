(ns ^{:doc "A TDD library for Clojure that supports top-down ('mockish') TDD, 
            encourages readable tests, provides a smooth migration path from 
            clojure.test, balances abstraction and concreteness, and strives for 
            graciousness."}
  midje.sweet
  (:use clojure.test
        [midje.util.namespace :only [immigrate intern+keep-meta]]
        midje.production-mode
        midje.error-handling.exceptions
        midje.error-handling.validation-errors
        midje.util.debugging
        [midje.util.form-utils :only [macro-for]]
        [midje.internal-ideas.wrapping :only [put-wrappers-into-effect]]
        [midje.internal-ideas.fact-context :only [nested-descriptions
                                                  within-runtime-fact-context]]
        [midje.internal-ideas.file-position :only [set-fallback-line-number-from]]
        [midje.ideas.tabular :only [tabular*]]
        [midje.ideas.facts :only [complete-fact-transformation future-fact*
                                  midjcoexpand 
                                  future-fact-variant-names]]
        [midje.ideas.compendium :only [dereference-history
                                       compendium-contents
                                       reset-compendium
                                       namespace-facts]]
        [midje.ideas.formulas :only [future-formula-variant-names]]
        [midje.ideas.metadata :only [separate-metadata]]
        [clojure.algo.monads :only [domonad]])
  (:require [midje.ideas.background :as background]
            [midje.ideas.formulas :as formulas]
            midje.checkers
            [midje.ideas.reporting.report :as report]))

(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

;; Following two are required because `intern` doesn't transfer "dynamicity".
(def ^{:doc "True by default.  If set to false, Midje checks are not
             included into production code, whether compiled or loaded."
       :dynamic true}
  *include-midje-checks* *include-midje-checks*)

(intern+keep-meta *ns* 'before  #'background/before)
(intern+keep-meta *ns* 'after   #'background/after)
(intern+keep-meta *ns* 'around  #'background/around)
(intern+keep-meta *ns* 'formula #'formulas/formula)

(defmacro background 
  "Runs facts against setup code which is run before, around, or after 
   :contents, :facts or :checks. Optionally, contains one or more provided forms 
   that apply for all facts until the end of the file. 
   All effects of `background` last until the end of the file."
  [& state-descriptions]
  (when (user-desires-checking?)
    (when-valid &form
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
    (domonad validate-m [_ (validate &form)
                         [metadata forms] (separate-metadata &form)]
      (try
        (set-fallback-line-number-from &form)
        (let [[background remainder] (background/separate-background-forms forms)]
          (if (seq background)
            `(against-background ~background (midje.sweet/fact ~@remainder))        	
            (complete-fact-transformation metadata remainder)))
        (catch Exception ex
          `(do
             (within-runtime-fact-context ~(:midje/description metadata)
               (clojure.test/report {:type :exceptional-user-error
                                     :description @nested-descriptions
                                     :macro-form '~&form
                                     :stacktrace '~(user-error-exception-lines ex)
                                     :position (midje.internal-ideas.file-position/line-number-known ~(:line (meta &form)))}))
             false))))))

(defmacro facts 
  "Alias for fact."
  [& forms]
  (when (user-desires-checking?)
    (when-valid &form
      (with-meta `(fact ~@forms) (meta &form)))))

(defmacro ^{:private true} generate-future-fact-variants []
  (macro-for [name future-fact-variant-names]
    `(defmacro ~(symbol name)
       "Fact that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (future-fact* ~'&form))))

(defmacro ^{:private true} generate-future-formula-variants []
  (macro-for [name future-formula-variant-names]
    `(defmacro ~(symbol name)
       "ALPHA/EXPERIMENTAL (subject to change)
        Formula that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (future-fact* ~'&form))))

(generate-future-fact-variants)
(generate-future-formula-variants)

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
  (tabular* (keys &env) &form))


(defmacro metaconstants
  "For a few operations, such as printing and equality checking,
   the Clojure AOT-compiler becomes confused by Midje's auto-generation
   of metaconstants. If AOT-compiled tests fail when on-the-fly
   compiled tests failed, declare your metaconstants before use.
   Example:
     (metaconstants ..m.. ..m.... .mc.)"
  [& names]
  (let [defs (map (fn [name]
                    `(def ~name (midje.ideas.metaconstants.Metaconstant. '~name {})))
                  names)]
    `(do ~@defs)))


(defn last-fact-checked
  "The last fact or tabular fact that was checked. Only top-level
   facts are recorded, not facts nested within them."
  []
  (dereference-history))

(defn recheck-fact 
  "Recheck the last fact or tabular fact that was checked.
   When facts are nested, the entire outer-level fact is rechecked."
  []
  ((last-fact-checked)))

(defn source-of-last-fact-checked 
  "Returns the source of the last fact or tabular fact run."
  []
  (:midje/source (meta (last-fact-checked))))

(defn forget-facts
  "After this, `check-facts` does nothing until new facts are defined."
  []
  (reset-compendium))

(defn check-facts
  "With no argument, checks all facts in the compendium.
   With arguments (namespaces or symbols), only runs facts
   in those namespaces. Returns true iff all facts check out."
  [& namespaces]
  (let [facts (if (empty? namespaces)
                (compendium-contents)
                (mapcat namespace-facts namespaces))]
        (every? true? (map #(%) facts))))
