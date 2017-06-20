(ns implementation.line-numbers.fim-check-failures
  (:require [midje.sweet :refer :all]
            [clojure.test :refer :all]
            [midje.test-util :refer :all]
            [midje.config :as config]
            [midje.emission.clojure-test-facade :as ctf]))

(defn f [n] n)
(defn g [n] n)




                   ;;; Simple cases

(def simple-start 16)

(silent-fact (+ 1 1) => 3)
(note-that (failure-was-at-line (+ simple-start 2)))

(silent-fact "odd positioning"

    (+ 1 1) =>   ; here

    3)
(note-that (failure-was-at-line (+ simple-start 7)))

(silent-fact (g 1) => 1
  (provided (f 2) => 2))   ; here
(note-that (failure-was-at-line (+ simple-start 13)))

(silent-fact (g 1) => 1

  ;; quite the gap.

  (provided
    (f 2) => 2))    ;; here
(note-that (failure-was-at-line (+ simple-start 21)))


               ;;; Inside a deftest

(unfinished favorite-animal)
(defn favorite-animal-name [] (name (favorite-animal)))
(defn return-nil [] )
(defn favorite-animal-only-animal [] (favorite-animal))
(defn favorite-animal-only-name [] (name "fred"))
(defn favorite-animal-one-call [] (name (favorite-animal 1)))

(def deftest-start 50)

(deftest unfolding-fakes-examples ;; Use a deftest to check that line numbers work inside.
  (fact
    (favorite-animal-name) => "betsy"
    (provided
      (name (favorite-animal)) => "betsy"))

  (silent-fact
    (return-nil) => "betsy"   ;; wrong result
    (provided
      (name (favorite-animal)) => "betsy"))  ;; two prerequisites unfolded here, but never called.

  (note-that (prerequisite-was-never-called #"name")
             (prerequisite-was-never-called #"favorite-animal")
             (fact-expected "betsy")
             (failures-were-at-lines  (+ deftest-start 11) (+ deftest-start 11) (+ deftest-start 9)))
  ;; Three things tested here. (1) there is a single composite "never-called" failure that
  ;; (2) has the position of the first of it's elements. Moreover, (3) the prerequisite failure
  ;; comes before the bad actual value.
  (for-failure 1 (note-that (failure-was-at-line (+ deftest-start 11))))
  (for-failure 2 (note-that (fact-expected "betsy")
                            (failure-was-at-line (+ deftest-start 9))))



  (silent-fact
   (favorite-animal-only-animal) => "betsy"  ;; here
   (provided
     (name (favorite-animal)) => "betsy"))   ;; here (name not called)

  (note-that (prerequisite-was-never-called #"name")
             (fact-expected "betsy")
             (failures-were-at-lines (+ deftest-start 29) (+ deftest-start 27)))

  (silent-fact
    (favorite-animal-only-name) => "betsy"   ;; This calls for the name of frank.
    (provided
      (name (favorite-animal)) => "betsy"))

  (note-that some-prerequisite-was-called-with-unexpected-arguments
             (prerequisite-was-never-called #"name")  ;; never correctly called, I guess I should say.
             (prerequisite-was-never-called #"favorite-animal")
             (fact-expected "betsy")
             (failures-were-at-lines (+ deftest-start 38) (+ deftest-start 38) (+ deftest-start 38) (+ deftest-start 36)))


  (silent-fact
    (favorite-animal-one-call) => "betsy"
    (provided
      (name (favorite-animal 1)) => "betsy"
      (name (favorite-animal 2)) => "jake")) ;; a folded prerequisite can have two errors.
  (note-that (failures-were-at-lines (+ deftest-start 51) (+ deftest-start 51)))
  (ctf/forget-failures) ;; So clojure.test failures don't show up in summary output.
  )



         ;;; Future facts

(config/with-augmented-config {:visible-future true}
  (capturing-fact-output
   (future-fact "text")
   (fact @fact-output => #"fim_check_failures.*112")))

(config/with-augmented-config {:visible-future true}
  (capturing-fact-output
   (pending-fact "text")
   (fact @fact-output => #"fim_check_failures.*117")))

(config/with-augmented-config {:visible-future true}
  (capturing-fact-output
   (fact "text"

     (+ 1 "1") =future=> "2")
   (fact @fact-output => #"fim_check_failures.*124")))




        ;;; Improved error handling for pathological cases

(def patho-start 132)

(silent-fact "statements without lists guess 1+ most recent"
   1 => even?)
(note-that (failure-was-at-line (+ patho-start 3)))

(silent-fact "that can cause mistakes"
                   ;; will appear to come from here
   1 => even?)
(note-that (failure-was-at-line (+ patho-start 7)))

(silent-fact "Facts that have detectable line numbers update the best-guess."

   (+ 1 2) => odd?  ;; best guess is now here
   1 => even?)      ;; so this is correctly reported
(note-that (failure-was-at-line (+ patho-start 14)))

(silent-fact "each emission, even if wrong, is taken as best guess for future"
   1 => even?  ;; This is correctly guessed.


   5 => even?  ;; This is not. Instead, 1 + the previous is reported.
   7 => even?  ;; So this is again guessed wrong: 2 + the first guess
   (+ 1 2) => odd?  ;; This will reset the counter
   3 => even?) ;; So this is a better guess
(let [start-of-fact (+ patho-start 17)]
  (note-that (failures-were-at-lines (+ 1 start-of-fact)
                                     (+ 2 start-of-fact)
                                     (+ 3 start-of-fact)
                                     (+ 7 start-of-fact))))

(silent-fact "old bug: a quoted form after an arrow would not put line numbers on following text"
  (list 1 2) => '(1 2)
  (+ 1 2) => 4)
(note-that (failure-was-at-line (+ patho-start 33)))


;;; Line number reporting for variant expect arrows

(def variant-line 171)
(silent-fact
 (+ 1 1) =deny=> 2
 (+ 1 1) =not=> 2)
(note-that (failures-were-at-lines (+ variant-line 2) (+ variant-line 3)))


(def tabular-line 178)
(tabular "The line number is the line number of the fact, not the substitutions."
  (silent-fact (inc ?n) => ?n)



  ?n  ?comment
  1   "1")
(note-that (failure-was-at-line (+ tabular-line 2)))

