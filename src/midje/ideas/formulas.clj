(ns ^{:doc "Midje's special blend of generative-style testing."}
  midje.ideas.formulas
  (:use [midje.util.form-utils :only [named? pop-docstring]]
        [midje.error-handling.validation-errors :only [simple-report-validation-error validate when-valid]]
        [midje.ideas.prerequisites :only [is-head-of-form-providing-prerequisites?]]
        [midje.ideas.arrows :only [leaves-contain-arrow? 
                                   leaf-expect-arrows]]))

(def ^{:doc "The number of facts generated per formula."
       :dynamic true} 
  *num-generations-per-formula* 100)   

(set-validator! #'*num-generations-per-formula* 
  (fn [new-val]
    (if (pos? new-val) 
      true
      (throw (RuntimeException. (str "*num-generations-per-formula* must be an integer 1 or greater. You tried to set it to: " new-val))))))


(defn shrink [& _args] [])

(defn- formula-fact [docstring body]
  `(midje.sweet/fact ~docstring   
     ~@body :formula :formula-in-progress))

(defmacro shrink-failure-case [docstring binding-name failed-binding-val body]
  `(loop [[cur-shrunk# & rest#] (midje.ideas.formulas/shrink ~failed-binding-val)]
     (when cur-shrunk#
       (when (let [~binding-name cur-shrunk#]
               ~(formula-fact docstring body))
         (recur rest#)))))

(defmacro formula 
  "ALPHA/EXPERIMENTAL - Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators
  
  The midje.ideas.formulas/*num-generations-per-formula* dynamic var determines
  how many facts are generated per formula."
  {:arglists '([docstring? bindings & body])}
  [& args]
  (when-valid &form
    (let [[docstring? [bindings & body]] (pop-docstring args)
          fact (formula-fact docstring? body)
          conclusion-signal `(midje.sweet/fact
                               :always-pass midje.sweet/=> :always-pass :formula :formula-conclude )]

      `(try
         (loop [cnt-down# midje.ideas.formulas/*num-generations-per-formula*]
           (when (pos? cnt-down#)
             (let [snd-binding# ~(second bindings)
                   ~(first bindings) snd-binding#]
               (if ~fact
                 (recur (dec cnt-down#))
                 (shrink-failure-case ~docstring? ~(first bindings) snd-binding# ~body)))))
         (finally
           ~conclusion-signal)))))

(defn- check-part-of [form] 
  (take-while #(not (and (named? %) (= "provided" (name %)))) 
              (flatten form)))

(defmethod validate "formula" [[_formula_ & args :as form]]
  (cond (not (leaves-contain-arrow? (check-part-of args)))
        (simple-report-validation-error form "There is no expection in your formula form:")
  
        (> (count (leaf-expect-arrows (check-part-of args))) 1)
        (simple-report-validation-error form "There are too many expections in your formula form:")

        (let [[_ [bindings & _]] (pop-docstring args)]
          (or (not (vector? bindings))
              (odd? (count bindings))
              (< (count bindings) 2)))
        (simple-report-validation-error form "Formula requires bindings to be an even numbered vector of 2 or more:")
  
        :else 
        args))