;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [midje.util.namespace :only [immigrate]]
        midje.production-mode
        midje.error-handling.monadic
        midje.util.debugging
        [midje.util.form-utils :only [macro-for]]
        [midje.util.exceptions :only [user-error-exception-lines]]
        [midje.internal-ideas.wrapping :only [put-wrappers-into-effect]]
        [midje.internal-ideas.fact-context :only [nested-fact-description]]
        [midje.internal-ideas.file-position :only [set-fallback-line-number-from]]
        [midje.ideas.tabular :only [tabular*]]
        [midje.ideas.facts :only [complete-fact-transformation future-fact* midjcoexpand 
                                  future-fact-variant-names]])
  (:require [midje.ideas.background :as background]
            midje.checkers
            [midje.util.report :as report]))

(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

;; Following is required because `intern` doesn't transfer "dynamicity".
(def ^{:dynamic true} *include-midje-checks* *include-midje-checks*)

(intern *ns* 'before #'background/before)
(intern *ns* 'after #'background/after)
(intern *ns* 'around #'background/around)

(defmacro background 
  "Runs facts against setup code which is run before, around, or after 
   :contents, :facts or :checks. Optionally, contains one or more provided forms 
   that apply for all facts until the end of the file. 
   All effects of `background` last until the end of the file."
  [& forms]
  (when (user-desires-checking?)
    (put-wrappers-into-effect (background/background-wrappers forms))))

(defmacro against-background 
  "Runs facts against setup code which is run before, around, or after 
   :contents, :facts or :checks. Optionally, contains one or more provided forms 
   that apply for all facts run within the against-background form. 
   All effects of `against-background` last until the end of the scope it surrounds."
  [background-forms & foreground-forms]
  (error-let [_ (validate &form)]
    (if (user-desires-checking?)
      (midjcoexpand `(against-background ~background-forms ~@foreground-forms))
     `(do ~@foreground-forms))))
    
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
  [& forms]
  (when (user-desires-checking?)
    (let [description (when (string? (first forms)) (first forms))]
      (try
        (set-fallback-line-number-from &form)
        (let [[background remainder] (background/separate-background-forms forms)]
          (if (seq background)
            `(against-background ~background (midje.sweet/fact ~@remainder))        	
            (complete-fact-transformation description remainder)))
        (catch Exception ex
          `(do
             (clojure.test/report {:type :exceptional-user-error
                                   :description ~description
                                   :macro-form '~&form
                                   :exception-lines '~(user-error-exception-lines ex)
                                   :position (midje.internal-ideas.file-position/line-number-known ~(:line (meta &form)))})
             false))))))

(defmacro facts 
  "Alias for fact."
  [& forms]
  (with-meta `(fact ~@forms) (meta &form)))

(defmacro ^{:private true} generate-future-fact-variants []
  (macro-for [name future-fact-variant-names]
    `(defmacro ~(symbol name)
       "Fact that will not be run. Generates 'WORK TO DO' report output as a reminder."
       [& forms#]
       (future-fact* ~'&form))))

(generate-future-fact-variants)

(defmacro tabular 
  "Generate a table of related facts.
  
   Ex. (tabular \"table of simple math\" 
         (fact (+ a b) => c)
           
           a b      c
           1 2      3
           3 4      7
           9 10     19 )"
  [& _]
  (tabular* (keys &env) &form))