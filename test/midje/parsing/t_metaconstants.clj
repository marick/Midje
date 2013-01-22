(ns midje.parsing.t-metaconstants
  (:use midje.parsing.metaconstants
        [midje sweet test-util]
        clojure.pprint)
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


