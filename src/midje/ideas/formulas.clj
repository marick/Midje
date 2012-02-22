(ns midje.ideas.formulas
  (:use [utilize.macro :only [macro-for]]))
                          
(def formula-run-bindings (atom {}))

;; add all of this to midje.sweet

(def #^:dynamic *num-generations* 100)

(defmacro formula [docstring? & bindings+body]
  (let [[docstring? bindings body] (if (string? docstring?) 
                                     [docstring? (first bindings+body) (rest bindings+body)]
                                     [nil docstring? bindings+body])]
    `(do
       (macro-for [_# (range (dec *num-generations*))]
         (let ~bindings
           ;; must I force the bindings to not use destructuring?
           ;; take left side of each binding, and its matching value and (reset! formula-run-bindings formula-binding-map)
           (midje.sweet/fact ~docstring?
             ~@body :formula :formula-in-progress )))
  
       (let ~bindings           
         ;; ditto
         (midje.sweet/fact ~docstring?
           ~@body :formula :formula-conclude )))))
