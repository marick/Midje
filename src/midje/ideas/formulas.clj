(ns midje.ideas.formulas
  (:use [utilize.macro :only [macro-for]]))

(def #^:dynamic *num-generations* 100)

(defmacro formula [bindings & body]
  `(do
     (macro-for [_# (range (dec *num-generations*))]
       (let ~bindings
         (midje.sweet/fact
           ~@body :formula :formula-in-progress )))

     (let ~bindings
       (midje.sweet/fact
         ~@body :formula :formula-conclude ))))
