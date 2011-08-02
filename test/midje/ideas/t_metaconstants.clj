;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-metaconstants
  (:use [midje.ideas.metaconstants :only [metaconstant-symbol? define-metaconstants
                                    with-fresh-generated-metaconstant-names
                                    metaconstant-for-form]])
  (:use midje.sweet)
  (:use midje.test-util)
)

(tabular 
 (fact "metaconstants begin and end with dots"
   '?candidate ?arrow metaconstant-symbol?)
 ?candidate   ?arrow
 ...foo...      => 
 .foo.          => 
 foo            =not=>
 .foo           =not=>
 foo.           =not=>
 "foo"          =not=>
 (..foo..)      =not=>)


(unfinished m)
(defn caller [head tail]
  (m head tail))

(fact "metaconstants work even when quoted"
  (caller 'sym ...tail...) => '(sym ...tail...)
  (provided (m 'sym ...tail...) => '(sym ...tail...)))

(defn claim-symbols [symbols]
  (doseq [metaconstant-symbol symbols]
    (find (ns-interns *ns*) metaconstant-symbol) => truthy
    (var-get ((ns-interns *ns*) metaconstant-symbol)) => metaconstant-symbol))

(fact "metaconstants are automatically defined"
  (define-metaconstants '(fact (f ...form...) => 1
                           [:in ...vec...]
                           {:in ...map...}
                           #{:in ...set...}))
  (claim-symbols '(...form... ...vec... ...map... ...set...)))


"Metaconstants can be declared in backgrounds"
(declare f)
(background (f ...one...) => 1 )
(against-background [ (f ...two...) => 2 ]
  (fact 
    (+ (f ...one...) (f ...two...) (f ...three...))  => 6
    (against-background (f ...three...) => 3)))
(claim-symbols '(...one... ...two... ...three...))

(fact "metaconstants can be created to stand in for an expression"
  (with-fresh-generated-metaconstant-names
    (metaconstant-for-form '(g)) => '...g-value-1...
    (metaconstant-for-form '(g)) => '...g-value-2...
    (metaconstant-for-form '(h)) => '...h-value-1...

    "Not fooled by namespaces"
    (metaconstant-for-form '(metaconstant-for-form))
    => '...metaconstant-for-form-value-1...))

