(ns ^{:doc "Midje's special blend of generative-style testing."}
  midje.parsing.0-to-fact-form.formulas
  (:use [midje.util.form-utils :only [first-named? named? pop-docstring pop-opts-map]]
        [midje.error-handling.validation-errors :only [simple-validation-error-report-form validate-m validate]]
        [midje.parsing.1-to-explicit-form.prerequisites :only [head-of-form-providing-prerequisites?]]
        [midje.parsing.util.arrows :only [leaf-expect-arrows leaves-contain-arrow?]]
        [midje.parsing.1-to-explicit-form.future-facts :only [future-prefixes]]
        [clojure.algo.monads :only [domonad]]
        [clojure.string :only [join]]
        [midje.util.form-utils :only [macro-for]]
        [clojure.walk :only [prewalk]])
  (:require [midje.emission.boundaries :as emission-boundary]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as emission-silence]))

;; Formulas work by running up to *num-trials* trials per formula.
(def ^{:doc "The number of trials generated per formula."
       :dynamic true} 
  *num-trials* 100)   

(set-validator! #'*num-trials* 
  (fn [new-val]
    (if (pos? new-val) 
      true
      (throw (Error. (str "*num-trials* must be an integer 1 or greater. You tried to set it to: " new-val))))))


(defn- formula-fact [docstring body]
  `(midje.sweet/fact ~docstring ~@body))

(defn- deconstruct-formula-args [args]
  (let [[docstring? more-args] (pop-docstring args)
        [opts-map [bindings & body]] (pop-opts-map more-args)]
    [docstring? opts-map bindings body]))



(defmacro around-formula [& body]
  `(do
     ~@body
     (if-let [failure# (last (state/raw-fact-failures))]
       (emit/fail failure#)
       (emit/pass))))

(defmacro formula 
  "Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators. 
   
  opts-map keys:
  
     :num-trials - Used to override the number of trials for this formula only. 
                   This is higher precedence than *num-trials*
                   Must be set to a number 1 or greater.
  
  The *num-trials* dynamic var determines
  how many facts are generated per formula."
  {:arglists '([docstring? opts-map? bindings & body])}
  [& _args]
  (domonad validate-m [[docstring? opts-map bindings body] (validate &form)
                       fact (formula-fact docstring? body)]

    `(around-formula
       (loop [num-trials-left# (or (:num-trials ~opts-map) midje.parsing.0-to-fact-form.formulas/*num-trials*)]
         (when (pos? num-trials-left#)
           (let [binding-rightsides# ~(vec (take-nth 2 (rest bindings)))
                 ~(vec (take-nth 2 bindings)) binding-rightsides#
                 success?# (emit/producing-only-raw-fact-failures ~fact)]

             (if success?#
               (recur (dec num-trials-left#))
               success?#)))))))

(defmacro with-num-trials [num-trials & formulas]
  `(binding [midje.parsing.0-to-fact-form.formulas/*num-trials* ~num-trials]
     ~@formulas))

(def future-formula-variant-names (map #(str % "formula") future-prefixes))


(defmacro generate-future-formula-variants []
  (macro-for [name future-formula-variant-names]
    `(defmacro ~(symbol name)
       "ALPHA/EXPERIMENTAL (subject to change)
        Formula that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (parse-future-fact/parse ~'&form))))

(defn- check-part-of [form]
  (prewalk (fn [form] 
             (if (some (partial first-named? form) ["against-background" "background" "provided"])
                 '() 
                 form)) 
    form))

(defmethod validate "formula" [[_formula_ & args :as form]]
  (let [[docstring? opts-map bindings body] (deconstruct-formula-args args)
        invalid-keys (remove (partial = :num-trials) (keys opts-map))]
    (cond (not (leaves-contain-arrow? (check-part-of args)))
          (simple-validation-error-report-form form "There is no expection in your formula form:")
    
          (> (count (leaf-expect-arrows (check-part-of args))) 1)
          (simple-validation-error-report-form form "There are too many expections in your formula form:")
  
          (or (not (vector? bindings))
              (odd? (count bindings))
              (< (count bindings) 2))
          (simple-validation-error-report-form form "Formula requires bindings to be an even numbered vector of 2 or more:")
  
          (some #(and (named? %) (= "background" (name %))) (flatten args))
          (simple-validation-error-report-form form "background cannot be used inside of formula")
  
          (not (empty? invalid-keys))
          (simple-validation-error-report-form form (format "Invalid keys (%s) in formula's options map. Valid keys are: :num-trials" (join ", " invalid-keys)))
          
          (and (:num-trials opts-map) 
               (not (pos? (:num-trials opts-map))))
          (simple-validation-error-report-form form (str ":num-trials must be an integer 1 or greater. You tried to set it to: " (:num-trials opts-map)))
      
          :else 
          [docstring? opts-map bindings body])))
