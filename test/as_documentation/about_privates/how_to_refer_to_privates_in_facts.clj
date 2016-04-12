(ns as-documentation.about-privates.how-to-refer-to-privates-in-facts
  (:require [midje.sweet :refer :all]
            [midje.util :refer :all]
            [midje.test-util :refer :all]))

;;; Suppose you want to test private vars. There are three ways to do it.

;; 1. A common Clojure idiom is to call a private through a var. That looks like this:

(require '[as-documentation.about-privates.privates-for-direct-access :as da])

(fact "calling private functions through their #'var"
  (fact "allows direct testing"
    (#'da/da-function-without-prerequisite 1) => 2)

  (fact "works with `provided`"
    (#'da/da-caller 1) => 8988
    (provided
      (#'da/da-called 1) => 8988)))


;; 2. Alternately, you can tag private vars with ^{:private true} metadata, and then
;;    "expose" them with `expose-testables`.

(require '[as-documentation.about-privates.privates-for-expose-testables])
(midje.util/expose-testables as-documentation.about-privates.privates-for-expose-testables)

(fact "a private has been exposed"
  (et-function-without-prerequisite 1) => 2)

(silent-fact "but you *cannot* use such exposed testables with `provided`"
  (et-caller 1) => 8988
  (provided
    (et-called 1) => 8988))
(note-that (fact-failed-with-note #"A prerequisite cannot use a symbol exposed via `expose-testables`")
           (fact-failed-with-note #"Instead, use the var directly: #'as-documentation.about-privates.privates-for-expose-testables/et-called"))


(fact "Here is how you must use them"
  (et-caller 1) => 8988
  (provided
    (#'as-documentation.about-privates.privates-for-expose-testables/et-called 1) => 8988))

;; 3. You can also name specific privates that should be made available in the current namespace.

(require '[as-documentation.about-privates.privates-for-testable-privates])
(midje.util/testable-privates as-documentation.about-privates.privates-for-testable-privates
                              tp-function-without-prerequisite tp-called tp-caller)

(fact "testable-privates allows direct testing"
  (tp-function-without-prerequisite 1) => 2)

(silent-fact "but such privates do note work with `provided`"
  (tp-caller 1) => 8988
  (provided
    (tp-called 1) => 8988))
(note-that (fact-failed-with-note #"A prerequisite cannot use a symbol exposed via.* `testable-privates`")
           (fact-failed-with-note #"Instead, use the var directly: #'as-documentation.about-privates.privates-for-testable-privates/tp-called"))

(fact "Here is how such a private must be used"
  (tp-caller 1) => 8988
  (provided
    (#'as-documentation.about-privates.privates-for-testable-privates/tp-called 1) => 8988))
