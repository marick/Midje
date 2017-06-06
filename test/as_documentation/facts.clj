(ns as-documentation.facts
  (:require [midje.config :as config]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; Midje's test suite is configured not to show some output that's demonstrated here.
;; This resets Midje's setting back to the default.
(config/with-augmented-config {:visible-future true}

                                            ;;; Basics

  ;; This is the simplest form of a fact. It has two *checkables*.
  (fact "addition works in Clojure"
    ;; Because addition works, we predict the following whenever Midje is run
    ;; for this file.
    (+ 10 10) => 20
    (+ 20 20) => 40)

  ;; Facts do not have to have descriptions
  (fact
    (+ 10 10) => 20
    (+ 20 20) => 40)

  (facts "`Facts` is a synonym for `fact`. It doesn't require multiple checkables."
    (+ 1 1) => 2)

  (silent-fact "Checkables fail individually."
    (+ 1 1) => 2
    (+ 2 2) => 3)
  (note-that (fails 1 time), (fact-expected 3), (fact-actual 4))

  ;; In some other test suites, exceptions caught when calculating the actual value
  ;; are counted separately as "errors". To Midje, they're just another kind of failure.
  (silent-fact
    (+ 1 "0") => 1
    (note-that fact-fails,
               (fact-captured-throwable-with-message #"Right side of =throws=> should extend Throwable")))


                                         ;;; Todos / future facts

  ;; Future facts act as TODO statements.
  (capturing-fact-output
   (future-fact "do something someday")
   (fact @fact-output => #"WORK TO DO.*do something someday"))

  (capturing-fact-output
   (fact "Individual checkables can be marked as 'not yet true'"
     (+ 1 1) => 2
     (- 1 1) =future=> 0)
   (fact
     @fact-output => (contains "WORK TO DO")
     @fact-output => (contains "on `(- 1 1)`")
     @fact-output => #"facts.clj:\d\d"))

)
