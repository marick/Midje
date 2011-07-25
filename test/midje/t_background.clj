;; -*- indent-tabs-mode: nil -*-

(ns midje.background
  (:require [clojure.zip :as zip])
  (:use [midje.midje-forms recognizing]
        [midje.error-handling monadic]
        [midje sweet test-util]))

(unfinished unused used)
(defn calls-nothing [] )

(unfinished local)
(defn calls-used [] (str (used) " " (local)))

(expect (separate-background-forms '[ (against-background) (f 1) => 3 ]) => [ [] '[ (f 1) => 3 ] ])



(tabular
 (fact "separate-background-forms divides forms into background and other things"
   (separate-background-forms '?in) => '[ ?background-forms  ?other-forms])

 ?in                                    ?background-forms               ?other-forms
 [ (against-background ..back1..
                       ..back2..)
   ..claim1..
   ..claim2..]                         [..back1.. ..back2..]          [..claim1.. ..claim2..]

 [ (against-background ..back1..)
   ..claim1..
   (against-background ..back2..)]      [..back1.. ..back2..]         [..claim1..] 

 []                                     []                              []
 [ (f 1) => 3 ]                         []                              [ (f 1) => 3 ]

 [(against-background)
  (f 1) => 3 ]                          []                              [(f 1) => 3]
)


(facts "dissecting setup/teardown forms"
  (setup-teardown-bindings '(before :checks (+ 1 1))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1), ?after nil})

  (setup-teardown-bindings '(before :checks (+ 1 1) :after (- 2 2))) =>
    (contains '{?key before, ?when :checks, ?first-form (+ 1 1),
                ?after :after, ?second-form (- 2 2)})

  (setup-teardown-bindings '(after :checks (+ 1 1))) =>
    (contains '{?key after, ?when :checks, ?first-form (+ 1 1)})

  (setup-teardown-bindings '(around :checks (let [x 1] ?form))) =>
    (contains '{?key around, ?when :checks,
                ?first-form (let [x 1] ?form) }))

