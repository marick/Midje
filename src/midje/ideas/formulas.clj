(ns midje.ideas.formulas)

(def #^:dynamic num-generations-per-formula 100)

(defmacro formula 
  "Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators
  
  For each formula the number of generated test runs is stored in the dynamic var 
  *num-generations-per-formula*, which is set to 100 by default."
  [docstring? & bindings+body]
  (let [[docstring? bindings body] (if (string? docstring?) 
                                     [docstring? (first bindings+body) (rest bindings+body)]
                                     [nil docstring? bindings+body])
        all-but-last-facts (repeat (dec num-generations-per-formula)
                             `(let ~bindings
                                (midje.sweet/fact ~docstring?
                                  ~@body :formula :formula-in-progress )))
        last-fact `(let ~bindings
                     (midje.sweet/fact ~docstring?
                       ~@body :formula :formula-conclude ))]
  
    `(do ~@all-but-last-facts ~last-fact)))
