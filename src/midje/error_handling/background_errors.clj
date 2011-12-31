;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.background-errors
  (:use
    [clojure.pprint :only [cl-format]]
    [midje.error-handling.monadic :only [simple-user-error-report-form user-error-report-form 
                                         validate when-valid]]
    [midje.ideas.arrows :only [is-start-of-checking-arrow-sequence? take-arrow-sequence]]
    [midje.ideas.background :only [all-state-descriptions seq-headed-by-setup-teardown-form?]]
    [midje.ideas.prerequisites :only [metaconstant-prerequisite?]]
    [midje.util.form-utils :only [named? pred-cond]]))

(def ^{:private true} valid-wrapping-targets #{:facts, :contents, :checks })

(defn- validate-state-description [[state-description wrapping-target expression :as form]]
  (cond 
      (and (#{"after" "around"} (name state-description)) (not= 3 (count form)))
      (user-error-report-form form
        (cl-format nil "    In this form: ~A" form)
        (cl-format nil "~A forms should look like: (~A :contents/:facts/:checks (your-code))" (name state-description) (name state-description))) 
  
      (and (= "before" (name state-description)) 
           (not= 3 (count form))
           (or (not= 5 (count form))
               (and (= 5 (count form)) 
                    (not= :after (nth form 3)))))
      (user-error-report-form form
        (cl-format nil "    In this form: ~A" form)      
        "before forms should look like: (before :contents/:facts/:checks (your-code)) or "
        "(before :contents/:facts/:checks (your-code) :after (final-code))")

      ((complement valid-wrapping-targets) wrapping-target)
      (user-error-report-form form
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

      (some-fn is-start-of-checking-arrow-sequence? metaconstant-prerequisite?) 
      (let [arrow-seq (take-arrow-sequence in-progress)]
        (recur (drop (count arrow-seq) in-progress)))
      
      seq-headed-by-setup-teardown-form?
      (recur (rest in-progress))
      
      :else
      false)))

(defn- state-description? [form]
  (and (sequential? form) 
       (all-state-descriptions (name (first form)))))

(defmethod validate "against-background" [[_against-background_ state-descriptions+fakes & _body_ :as form]]
  (cond (< (count form) 3)
        (simple-user-error-report-form form
          "You need a minimum of three elements to an against-background form:")
     
        (vector? state-descriptions+fakes)                                    
        (when-valid (filter state-description? state-descriptions+fakes)
          (pred-cond state-descriptions+fakes   
            empty?
            (simple-user-error-report-form form
              "You didn't enter any background fakes or wrappers:")
          
            (comp not valid-state-descriptions+fakes?)
            (simple-user-error-report-form form
              "Badly formatted against-background fakes:")
      
            :else
            (rest form)))
          
        (sequential? state-descriptions+fakes)
        (if (named? (first state-descriptions+fakes))
          (when-valid state-descriptions+fakes (rest form))
          (rest form))
          
        :else                                      
        (simple-user-error-report-form form 
          "Malformed against-background. against-background requires"
          "at least one background fake or background wrapper: " )))

(defmethod validate "background" [[_background_ & state-descriptions+fakes :as form]]
  (when-valid (filter state-description? state-descriptions+fakes) 
    (pred-cond state-descriptions+fakes 
      empty?
      (simple-user-error-report-form form "You didn't enter any background fakes or wrappers:")
  
      (comp not valid-state-descriptions+fakes?)
      (simple-user-error-report-form form "Badly formatted background fakes:")
      
      :else
      state-descriptions+fakes)))