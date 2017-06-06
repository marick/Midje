(ns user.fus-nested-backgrounds
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; It's proven tricky to get the nesting of the macroexpansion of backgrounds correct. Hence these tests.

(unfinished f)

;; First, a little demonstration that deeply-nested errors don't get lost.

(silent-fact
  (against-background [(f 1) => 1]
    (fact
      (against-background [(f 2) => 2]
        (fact
          (against-background f => 3)
          (+ (f 1) (f 2)) => 3)))))
(note-that parse-error-found (fact-failed-with-note #"must look like a function call"))

;;; The nesting cases

(fact
  (fact
    (against-background (f 1) => 1)
    (f 1) => 1))

(fact
  (against-background [(f 1) => 1]
    (fact
      (f 1) => 1)))

(fact
  (against-background [(f 1) => 1]
    (fact
      (against-background [(f 2) => 2]
        (fact
          (+ (f 1) (f 2)) => 3)))))

(fact
  (against-background (f 1) => 1)
  (fact
    (against-background (f 2) => 2)
    (+ (f 1) (f 2)) => 3))

(fact
  (against-background (f 1) => 1)
  (fact
    (against-background [(f 2) => 2]
      (fact
        (+ (f 1) (f 2)) => 3))))

(fact
  (against-background [(f 1) => 1]
    (fact
      (against-background (f 2) => 2)
      (+ (f 1) (f 2)) => 3)))


(against-background [(f 1) => 1]
  (fact
    (against-background (f 2) => 2)
    (+ (f 1) (f 2)) => 3))


(against-background [(f 1) => 1]
  (fact
    (against-background [(f 2) => 2]
      (fact
        (+ (f 1) (f 2)) => 3))))

;; Some of the new looser forms of against-background


(fact
  (against-background [(f 1) => 1]
    (fact
      (against-background [(f 2) => 2])
      (+ (f 1) (f 2)) => 3)))

(fact
  (background [(f 1) => 1])
  (fact
    (background (f 2) => 2)
    (+ (f 1) (f 2)) => 3))

;;; Want to show that error cases work as well.

(silent-fact
  (against-background [(f 1) => 1]
    (against-background [(f 2) => 2]
      (fact
        (+ (f 1) (f 2)) => 333)))

  (fact
    (against-background [(f 2) => 3]
      (fact
        (against-background [(f 3) => 4])
        (+ (f 2) (f 3)) => 777))))
(note-that fact-fails)
(for-failure 1 (note-that (fact-actual 3) (fact-expected 333)))
(for-failure 2 (note-that (fact-actual 7) (fact-expected 777)))



