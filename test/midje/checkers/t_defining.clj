;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-defining
  (:use midje.sweet
        [midje.checkers.defining :only [checker?]]
        midje.test-util))

(defchecker magic-number "docstring" [actual] (= 587 actual))
(fact
  magic-number => checker?
  #'magic-number => checker?
  587 => magic-number
  (magic-number 8) => falsey
  (:doc (meta #'magic-number)) => "docstring")

(defchecker undocumented-magic-number [actual] (= 587 actual))
(fact
  undocumented-magic-number => checker?
  #'undocumented-magic-number => checker?
  587 => undocumented-magic-number
  (undocumented-magic-number 8) => falsey
  (:doc (meta #'undocumented-magic-number)) => nil)

(fact
  (let [inline-magic-number (checker [actual] (= 587 actual))]
    inline-magic-number => checker?
    587 => inline-magic-number
    (inline-magic-number 8) => falsey))


(fact "checkers can be recognized"
  #'midje.checkers.simple/truthy => checker?
  (checker? #'clojure.core/odd?) => falsey

  "sloop" => (fn [actual] (= "sloop" actual)) ; an inline checker
  (checker? (fn [actual] (= "sloop" actual))) => falsey
  (tag-as-checker (fn [actual] (= "sloop" actual))) => checker?

  (checker? "blah") => falsey
  (checker? 1) => falsey)

