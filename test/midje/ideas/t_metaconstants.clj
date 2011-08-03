;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-metaconstants
  (:use midje.ideas.metaconstants
        [midje sweet test-util])
  (:require [clojure.zip :as zip])
  (:import midje.ideas.metaconstants.Metaconstant))

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



(fact "Metaconstants print as their name"
  (let [mc (Metaconstant. '...name... {})]
    (str mc) => "...name..."
    (pr-str mc) => "...name..."))

(fact "Metaconstants are equal if their *names* are equal."
  (Metaconstant.    '...name... {:key "value"}) 
  => (Metaconstant. '...name... {:key "not-value"})
  "And they are equal to symbols with that name"
  (= (Metaconstant. '...name... {}) '...name...) => truthy
  (= (Metaconstant. '...name... {}) '...not-name...) => falsey
  "... which has this implication:"
  (list 'a (Metaconstant. '...name... {})) => '(a ...name...)
  '(a ...name...) => (list 'a (Metaconstant. '...name... {})))
  

(fact "Metaconstants implement ILookup"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (:key mc) => "value"
    (:not-key mc "default") => "default"
    "And let's allow the other type of map lookup"
    (mc :key) => "value"
    (mc :not-key "default") => "default"))

(fact "Metaconstants implement Associative"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (contains? mc :key) => truthy
    (contains? mc :not-key) => falsey

    (find mc :key) => [:key "value"]

    (let [new-mc (assoc mc :new-key "new value")]
      (type new-mc) => Metaconstant
      (:new-key new-mc) => "new value"
      "Note that the new metaconstant is equal to the old!"
      (= new-mc new-mc))))
    
(fact "Associate extends Seqable and IPersistentCollection"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (seq mc) => (seq {:key "value"})
    (count mc) => 1
    (empty? mc) => falsey
    (.equiv mc mc) => truthy))
    


  

    

  


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

