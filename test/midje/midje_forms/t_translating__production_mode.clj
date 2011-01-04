;; -*- indent-tabs-mode: nil -*-

(ns midje.midje-forms.t-translating--production-mode
  (:use [midje.sweet])
  (:use clojure.contrib.pprint))

(alter-var-root #'*include-midje-checks* (constantly false))

(fact (/ 1 0) => 33)
(facts (/ 1 0) => 33)
(background (broken-form ...meta...) =>)
(fact (+ 1 (broken-form ...meta...) => 33))

(against-background [(broken-form2 ...meta2...) =>]
                    (fact (+ 1 (broken-form-2 ...meta2...)) => 33)
                    (def *localvar* "this must nevertheless be defined"))

(str *localvar*)  ;; Non-fact forms are still processed.

(alter-var-root #'*include-midje-checks* (constantly true))
