(ns midje.parsing.1-to-explicit-form.t-metaconstants
  (:require [midje.parsing.1-to-explicit-form.metaconstants :refer :all]
            [midje
             [sweet :refer :all]
             [test-util :refer :all]])
  (:import midje.data.metaconstant.Metaconstant))

;;; Two different ways of creating metaconstants from user forms

(predefine-metaconstants-from-form
 '(fact (f ...form...) => 1
    [:in ...vec...]
    {:in ...map...}
    #{:in ...set...}))

(def types (set (map type [...form... ...vec... ...map... ...set...])))
(fact types => #{midje.data.metaconstant.Metaconstant})

(def names (map name [...form... ...vec... ...map... ...set...]))
(fact names => ["...form..." "...vec..." "...map..." "...set..."])


(metaconstants ..a.. --b--)
(def types (set (map type [..a.. --b--])))
(fact types => #{midje.data.metaconstant.Metaconstant})

(def names (map name [..a.. --b--]))
(fact names => ["..a.." "--b--"])


;;; Some random other stuff.

(defn concer [source] (str (:a source) (:b source) (:c source)))
(fact "metaconstant parsing obeys lexical scoping"
  (let [c 'c]
    (concer ...source...) => "abc"
    (provided
      ...source... =contains=> {:a 'a, :b 'b}
      ...source... =contains=> {:c c})))


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

"Metaconstants can be declared in backgrounds"
(declare f)
(background (f ...one...) => 1 )
(against-background [ (f ...two...) => 2 ]
  (fact
    (+ (f ...one...) (f ...two...) (f ...three...))  => 6
    (against-background (f ...three...) => 3)))
(claim-symbols '(...one... ...two... ...three...))

