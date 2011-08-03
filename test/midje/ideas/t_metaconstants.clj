;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-metaconstants
  (:use midje.ideas.metaconstants
        [midje sweet test-util])
  (:require [clojure.zip :as zip]))

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
  (fact 
    (doseq [metaconstant-symbol symbols]
      (find (ns-interns *ns*) metaconstant-symbol) => truthy
      (var-get ((ns-interns *ns*) metaconstant-symbol)) => metaconstant-symbol)))

(define-metaconstants '(fact (f ...form...) => 1
                         [:in ...vec...]
                         {:in ...map...}
                         #{:in ...set...}))
(claim-symbols '(...form... ...vec... ...map... ...set...))


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


(defn incer [thing] (inc (:value thing)))

(fact "metaconstants have a function that is used to fake lookup"
  (+ 1 (midje.ideas.metaconstants/meta-get ...metaconst... :value)) => 2
  (provided
    (midje.ideas.metaconstants/meta-get ...metaconst... :value) => 1))


(let [key-first-variants (map zip/seq-zip '[(:value ...meta...)
                                            (:value ...meta... default)
                                            (:value not-meta)
                                            (not-key ...meta...)])]
  (fact 
    (map key-first-lookup? key-first-variants)
    => (just [truthy truthy falsey falsey])))
      
(let [meta-first-variants (map zip/seq-zip '[(...meta... anything)
                                             (not-meta anything)
                                             (...meta... anything default)])]
  (fact
    (map meta-first-lookup? meta-first-variants)
    => (just [truthy falsey truthy])))
      

(fact
  (metaconstant-lookup-transform '(f (...mc... :value)))
  => '(f (midje.ideas.metaconstants/meta-get ...mc... :value))

  (metaconstant-lookup-transform '(f (...mc... :value :default)))
  => '(f (midje.ideas.metaconstants/meta-get ...mc... :value :default))

  (metaconstant-lookup-transform '(f (:value ...mc...)))
  => '(f (midje.ideas.metaconstants/meta-get ...mc... :value))
  (metaconstant-lookup-transform '(f (:value ...mc... 1)))
  => '(f (midje.ideas.metaconstants/meta-get ...mc... :value 1)))



