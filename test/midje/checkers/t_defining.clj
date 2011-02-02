;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-defining
  (:use midje.sweet
        [midje.checkers.defining :only [checker?]]
        midje.test-util))

(defchecker magic-number "magic number docstring" [actual] (= 587 actual))
(fact "checker with a doc string"
  magic-number => checker?
  #'magic-number => checker?
  587 => magic-number
  (magic-number 8) => falsey
  (:arglists (meta #'magic-number)) => '([actual])
  (:doc (meta #'magic-number)) => "magic number docstring")

(defchecker undocumented-magic-number [actual] (= 587 actual))
(fact "checker without a docstring"
  undocumented-magic-number => checker?
  #'undocumented-magic-number => checker?
  587 => undocumented-magic-number
  (undocumented-magic-number 8) => falsey
  (:arglists (meta #'undocumented-magic-number)) => '([actual])
  (:doc (meta #'undocumented-magic-number)) => nil)

(defchecker double-magic-number "double docstring"
  ([actual] (= 587 actual))
  ([actual increment] (= (+ actual increment) 587)))
(fact "checker with a doc string and more than one function body"
  double-magic-number => checker?
  #'double-magic-number => checker?
  587 => double-magic-number
  (double-magic-number 8) => falsey
  (double-magic-number 587 0) => truthy
  (double-magic-number 587 1) => falsey
  (:arglists (meta #'double-magic-number)) => '([actual] [actual increment])
  (:doc (meta #'double-magic-number)) => "double docstring")

(defchecker double-magic-number-no-docstring "double docstring"
  ([actual] (= 587 actual))
  ([actual increment] (= (+ actual increment) 587)))
(fact "checker with a doc string and more than one function body"
  double-magic-number-no-docstring => checker?
  #'double-magic-number-no-docstring => checker?
  587 => double-magic-number-no-docstring
  (double-magic-number-no-docstring 8) => falsey
  (double-magic-number-no-docstring 587 0) => truthy
  (double-magic-number-no-docstring 587 1) => falsey
  (:arglists (meta #'double-magic-number-no-docstring)) => '([actual] [actual increment]))

(fact "inline checker"
  (let [inline-magic-number (checker [actual] (= 587 actual))]
    inline-magic-number => checker?
    587 => inline-magic-number
    (inline-magic-number 8) => falsey))


(fact "checkers can be recognized"
  #'midje.checkers.simple/truthy => checker?
  (checker? #'clojure.core/odd?) => falsey

  "sloop" => (fn [actual] (= "sloop" actual)) ; an inline checker
  (checker? (fn [actual] (= "sloop" actual))) => falsey
  (as-checker (fn [actual] (= "sloop" actual))) => checker?

  (checker? "blah") => falsey
  (checker? 1) => falsey)
