(ns ^{:doc "Validation methods confirming the proper syntax of (against-)background macros."}
  midje.error-handling.background-validations
  (:use [clojure.pprint :only [cl-format]]
        [midje.error-handling.validation-errors :only [simple-validation-error-report-form 
                                                       validation-error-report-form validate when-valid]]
        [midje.ideas.arrows :only [start-of-checking-arrow-sequence? take-arrow-sequence]]
        [midje.ideas.background :only [seq-headed-by-setup-teardown-form?]]
        [midje.ideas.prerequisites :only [metaconstant-prerequisite?]]
        [midje.util.form-utils :only [def-many-methods named? pred-cond]]
        [midje.util.backwards-compatible-utils :only [some-fn-m]]))

(def #^:private possible-wrapping-targets   #{:facts, :contents, :checks })
(def #^:private possible-state-descriptions #{"before" "after" "around"})

(def-many-methods validate ["before" "after" "around"] [[state-description wrapping-target expression :as form]]
  (cond
    (and (#{"after" "around"} (name state-description)) (not= 3 (count form)))
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      (cl-format nil "~A forms should look like: (~A :contents/:facts/:checks (your-code))"
        (name state-description) (name state-description)))

    (and (= "before" (name state-description))
         (not= 3 (count form))
         (or (not= 5 (count form))
             (and (= 5 (count form))
                  (not= :after (nth form 3)))))
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      "before forms should look like: (before :contents/:facts/:checks (your-code)) or "
      "(before :contents/:facts/:checks (your-code) :after (final-code))")

    ((complement possible-wrapping-targets) wrapping-target)
    (validation-error-report-form form
      (cl-format nil "    In this form: ~A" form)
      (cl-format nil "The second element (~A) should be one of: :facts, :contents, or :checks"
        wrapping-target))

    :else (rest form)))

(letfn [(possible-state-descriptions+fakes? [forms]
          (loop [in-progress forms]
            (pred-cond in-progress
              empty?
              true

              (some-fn-m start-of-checking-arrow-sequence? metaconstant-prerequisite?)
              (let [arrow-seq (take-arrow-sequence in-progress)]
                (recur (drop (count arrow-seq) in-progress)))

              seq-headed-by-setup-teardown-form?
              (recur (rest in-progress))

              :else false)))

        (state-description? [form]
          (and (sequential? form)
            (contains? possible-state-descriptions (name (first form))))) ]

  (defmethod validate "against-background" 
    [[_against-background_ state-descriptions+fakes & _body_ :as form]]
    (cond (< (count form) 3)
          (simple-validation-error-report-form form
            "You need a minimum of three elements to an against-background form:")
       
          (vector? state-descriptions+fakes)                                    
          (when-valid (filter state-description? state-descriptions+fakes)
            (pred-cond state-descriptions+fakes   
              empty?
              (simple-validation-error-report-form form
                "You didn't enter any background fakes or wrappers:")
            
              (comp not possible-state-descriptions+fakes?)
              (simple-validation-error-report-form form
                "Badly formatted against-background fakes:")
        
              :else
              (rest form)))
            
          (sequential? state-descriptions+fakes)
          (if (named? (first state-descriptions+fakes))
            (when-valid state-descriptions+fakes (rest form))
            (rest form))
            
          :else                                      
          (simple-validation-error-report-form form 
            "Malformed against-background. against-background requires"
            "at least one background fake or background wrapper: " )))
  
  (defmethod validate "background" [[_background_ & state-descriptions+fakes :as form]]
    (when-valid (filter state-description? state-descriptions+fakes) 
      (pred-cond state-descriptions+fakes 
        empty?
        (simple-validation-error-report-form form "You didn't enter any background fakes or wrappers:")
    
        (comp not possible-state-descriptions+fakes?)
        (simple-validation-error-report-form form "Badly formatted background fakes:")
        
        :else
        state-descriptions+fakes))))