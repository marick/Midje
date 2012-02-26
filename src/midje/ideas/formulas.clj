(ns midje.ideas.formulas
  (:use [midje.util.form-utils :only [pop-docstring]]))

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
  (let [[docstring? [bindings & body]] (pop-docstring args)
        all-but-last-facts (repeat (dec num-generations-per-formula)
                             `(let ~bindings
                                (midje.sweet/fact ~docstring?
                                  ~@body :formula :formula-in-progress )))
        last-fact `(let ~bindings
                     (midje.sweet/fact ~docstring?
                       ~@body :formula :formula-conclude ))]
  
    `(do ~@all-but-last-facts ~last-fact)))
