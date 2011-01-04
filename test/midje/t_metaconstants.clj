;; -*- indent-tabs-mode: nil -*-

(ns midje.t-metaconstants
  (:use [midje.metaconstants :only [metaconstant? define-metaconstants]])
  (:use midje.sweet)
  (:use midje.test-util)
)

(fact "metaconstants begin and end with dots"
  (metaconstant? '...foo...) => truthy
  (metaconstant? '.foo.) => truthy
  (metaconstant? 'foo) => falsey
  (metaconstant? '.foo) => falsey
  (metaconstant? 'foo.) => falsey)

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

