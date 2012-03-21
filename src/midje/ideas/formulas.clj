(ns ^{:doc "Midje's special blend of generative-style testing."}
  midje.ideas.formulas
  (:use [midje.util.form-utils :only [first-named? named? pop-docstring pop-opts-map]]
        [midje.error-handling.validation-errors :only [simple-report-validation-error 
                                                       validate valid-let]]
        [midje.ideas.prerequisites :only [is-head-of-form-providing-prerequisites?]]
        [midje.ideas.arrows :only [leaf-expect-arrows leaves-contain-arrow?]]
        [midje.ideas.facts :only [future-prefixes]]
        [clojure.string :only [join]]
        [clojure.walk :only [prewalk]]))

(def ^{:doc "The number of trials generated per formula."
       :dynamic true} 
  *num-trials* 100)   

(set-validator! #'*num-trials* 
  (fn [new-val]
    (if (pos? new-val) 
      true
      (throw (RuntimeException. (str "*num-trials* must be an integer 1 or greater. You tried to set it to: " new-val))))))


(defn shrink [& _args] [])

(defn- formula-fact [docstring body]
  `(midje.sweet/fact ~docstring   
     ~@body :formula :formula-in-progress))

(defmacro shrink-failure-case [docstring binding-leftsides failed-binding-rightsides body]
  `(loop [shrunk-binding-rightsides# (map midje.ideas.formulas/shrink ~failed-binding-rightsides)]
     (let [cur-shrunks# (map first shrunk-binding-rightsides#)]
       (when (and (first cur-shrunks#)
                  (let [~binding-leftsides cur-shrunks#]
                    ~(formula-fact docstring body)))
           (recur (map rest shrunk-binding-rightsides#))))))

(defn- deconstruct-formula-args [args]
  (let [[docstring? more-args] (pop-docstring args)
        [opts-map [bindings & body]] (pop-opts-map more-args)]
    [docstring? opts-map bindings body]))

(defmacro formula 
  "ALPHA/EXPERIMENTAL (subject to change) - Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators. 
  (However we are in the works to create a library of generators with shrinkers, so 
   don't get too attached to test.generative)
   
  opts-map keys:
  
     :num-trials - Used to override the number of trials for this formula only. 
                   This is higher precedence than *num-trials*
                   Must be set to a number 1 or greater.
  
  The midje.ideas.formulas/*num-trials* dynamic var determines
  how many facts are generated per formula."
  {:arglists '([docstring? opts-map? bindings & body])}
  [& args]
  (valid-let [[docstring? opts-map bindings body] (validate &form)
              fact (formula-fact docstring? body)
              conclusion-signal `(midje.sweet/fact
                                   :always-pass midje.sweet/=> :always-pass 
                                   :formula :formula-conclude )]

    `(try
       (loop [num-trials-left# (or (:num-trials ~opts-map) midje.ideas.formulas/*num-trials*)]
         (when (pos? num-trials-left#)
           (let [binding-rightsides# ~(vec (take-nth 2 (rest bindings)))
                 ~(vec (take-nth 2 bindings)) binding-rightsides#]
             (if ~fact
               (recur (dec num-trials-left#))
               (shrink-failure-case ~docstring? 
                                    ~(vec (take-nth 2 bindings)) 
                                    binding-rightsides# 
                                    ~body)))))
       (finally
         ~conclusion-signal))))

(defmacro with-num-trials [num-trials & formulas]
  `(binding [midje.ideas.formulas/*num-trials* ~num-trials]
     ~@formulas))

(def future-formula-variant-names (map #(str % "formula") future-prefixes))

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
          (simple-report-validation-error form "There is no expection in your formula form:")
    
          (> (count (leaf-expect-arrows (check-part-of args))) 1)
          (simple-report-validation-error form "There are too many expections in your formula form:")
  
          (or (not (vector? bindings))
              (odd? (count bindings))
              (< (count bindings) 2))
          (simple-report-validation-error form "Formula requires bindings to be an even numbered vector of 2 or more:")
  
          (some #(and (named? %) (= "background" (name %))) (flatten args))
          (simple-report-validation-error form "background cannot be used inside of formula")
  
          (not (empty? invalid-keys))
          (simple-report-validation-error form (format "Invalid keys (%s) in formula's options map. Valid keys are: :num-trials" (join ", " invalid-keys)))
          
          (and (:num-trials opts-map) 
               (not (pos? (:num-trials opts-map))))
          (simple-report-validation-error form (str ":num-trials must be an integer 1 or greater. You tried to set it to: " (:num-trials opts-map)))
      
          :else 
          [docstring? opts-map bindings body])))