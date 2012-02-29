(ns midje.ideas.formulas
  (:use [midje.util.form-utils :only [pop-docstring]]
        [midje.error-handling.validation-errors :only [simple-report-validation-error validate when-valid]]
        [midje.ideas.arrows :only [leaves-contain-arrow?]]))

(def ^{:private true} num-generations-per-formula 100)

(defmacro formula 
  "ALPHA - Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators
  
  For each formula there are 100 generated test runs."
  {:arglists '([docstring? & bindings+body])}
  [& args]
  (when-valid &form
    (let [[docstring? [bindings & body]] (pop-docstring args)
          all-but-last-facts (repeat (dec num-generations-per-formula)
                               `(let ~bindings
                                  (midje.sweet/fact ~docstring?
                                    ~@body :formula :formula-in-progress )))
          last-fact `(let ~bindings
                       (midje.sweet/fact ~docstring?
                         ~@body :formula :formula-conclude ))]
    
      `(do ~@all-but-last-facts ~last-fact))))

(defmethod validate "formula" [[_formula_ & args :as form]]
  (cond (not (leaves-contain-arrow? args))
        (simple-report-validation-error form "There is no arrow in your formula form:")

        (let [bindings (first (second (pop-docstring args)))]
          (or (not (vector? bindings)) (odd? (count bindings))))
        (simple-report-validation-error form "Formula requires bindings be an even numbered vector of 2 or more:")
  
        :else 
        args))