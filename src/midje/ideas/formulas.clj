(ns midje.ideas.formulas
  (:use [utilize.macro :only [macro-for]]))

(def #^:dynamic *num-generations* 100)

(defmacro formula [docstring? & bindings+body]
  (let [[docstring? bindings body] (if (string? docstring?) 
                                     [docstring? (first bindings+body) (rest bindings+body)]
                                     [nil docstring? bindings+body])]
    `(do
       (macro-for [_# (range (dec *num-generations*))]
         (let ~bindings
           (midje.sweet/fact ~docstring?
             ~@body :formula :formula-in-progress )))
  
       (let ~bindings
         (midje.sweet/fact ~docstring?
           ~@body :formula :formula-conclude )))))
