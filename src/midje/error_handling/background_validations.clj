;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Validation methods confirming the proper syntax of (against-)background macros."}
  midje.error-handling.background-validations
  (:use
    [clojure.pprint :only [cl-format]]
    [midje.error-handling.validation-errors :only [simple-report-validation-error report-validation-error 
                                         validate when-valid]]
    [midje.ideas.arrows :only [is-start-of-checking-arrow-sequence? take-arrow-sequence]]
    [midje.ideas.background :only [seq-headed-by-setup-teardown-form?]]
    [midje.ideas.prerequisites :only [metaconstant-prerequisite?]]
    [midje.util.form-utils :only [named? pred-cond]]
    [midje.util.backwards-compatible-utils :only [some-fn-m]]))

(def ^:private valid-wrapping-targets #{:facts, :contents, :checks })

(defn- validate-state-description [[state-description wrapping-target expression :as form]]
  (cond 
      (and (#{"after" "around"} (name state-description)) (not= 3 (count form)))
      (report-validation-error form
        (cl-format nil "    In this form: ~A" form)
        (cl-format nil "~A forms should look like: (~A :contents/:facts/:checks (your-code))" (name state-description) (name state-description))) 
  
      (and (= "before" (name state-description)) 
           (not= 3 (count form))
           (or (not= 5 (count form))
               (and (= 5 (count form)) 
                    (not= :after (nth form 3)))))
      (report-validation-error form
        (cl-format nil "    In this form: ~A" form)      
        "before forms should look like: (before :contents/:facts/:checks (your-code)) or "
        "(before :contents/:facts/:checks (your-code) :after (final-code))")

      ((complement valid-wrapping-targets) wrapping-target)
      (report-validation-error form
        (cl-format nil "    In this form: ~A" form)
        (cl-format nil "The second element (~A) should be one of: :facts, :contents, or :checks" wrapping-target))

      :else
      (rest form)))   

(defmethod validate "before" [forms]
  (validate-state-description forms))

(defmethod validate "after" [forms]
  (validate-state-description forms))

(defmethod validate "around" [forms]
  (validate-state-description forms))

(defn- valid-state-descriptions+fakes? [forms]
  (loop [in-progress forms]
    (pred-cond in-progress
      empty? 
      true

      (some-fn-m is-start-of-checking-arrow-sequence? metaconstant-prerequisite?) 
      (let [arrow-seq (take-arrow-sequence in-progress)]
        (recur (drop (count arrow-seq) in-progress)))
      
      seq-headed-by-setup-teardown-form?
      (recur (rest in-progress))
      
      :else
      false)))

(def ^:private valid-state-descriptions #{"before" "after" "around"}) 

(defn- state-description? [form]
  (and (sequential? form) 
       (valid-state-descriptions (name (first form)))))

(defmethod validate "against-background" [[_against-background_ state-descriptions+fakes & _body_ :as form]]
  (cond (< (count form) 3)
        (simple-report-validation-error form
          "You need a minimum of three elements to an against-background form:")
     
        (vector? state-descriptions+fakes)                                    
        (when-valid (filter state-description? state-descriptions+fakes)
          (pred-cond state-descriptions+fakes   
            empty?
            (simple-report-validation-error form
              "You didn't enter any background fakes or wrappers:")
          
            (comp not valid-state-descriptions+fakes?)
            (simple-report-validation-error form
              "Badly formatted against-background fakes:")
      
            :else
            (rest form)))
          
        (sequential? state-descriptions+fakes)
        (if (named? (first state-descriptions+fakes))
          (when-valid state-descriptions+fakes (rest form))
          (rest form))
          
        :else                                      
        (simple-report-validation-error form 
          "Malformed against-background. against-background requires"
          "at least one background fake or background wrapper: " )))

(defmethod validate "background" [[_background_ & state-descriptions+fakes :as form]]
  (when-valid (filter state-description? state-descriptions+fakes) 
    (pred-cond state-descriptions+fakes 
      empty?
      (simple-report-validation-error form "You didn't enter any background fakes or wrappers:")
  
      (comp not valid-state-descriptions+fakes?)
      (simple-report-validation-error form "Badly formatted background fakes:")
      
      :else
      state-descriptions+fakes)))