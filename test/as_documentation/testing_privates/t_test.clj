(ns as-documentation.testing-privates.t-test
  (:use midje.sweet
        midje.util
        midje.test-util))

;;; Calling private functions through vars (a common clojure idiom)

(require '[as-documentation.testing-privates.privates-for-direct-access :as da])

(fact "calling private functions through their #'var"
  (fact "allows direct testing"
    (#'da/da-function-without-prerequisite 1) => 2)

  (fact "works with `provided`"
    (#'da/da-caller 1) => 8988
    (provided
      (#'da/da-called 1) => 8988)))

;;; Creating local vars for all privates marked testable.

(require '[as-documentation.testing-privates.privates-for-expose-testables])
(midje.util/expose-testables as-documentation.testing-privates.privates-for-expose-testables)

(fact "expose-testables interns some privates in this namespace"
  (fact "allows direct testing"
    (et-function-without-prerequisite 1) => 2)

  (after-silently
   (fact "DOES NOT works with `provided`"
     (et-caller 1) => 8988
     (provided
       (et-called 1) => 8988))
   (fact (first @reported) => a-validation-error)))
  
  
;;; Creating local vars for all privates in another namespace

(require '[as-documentation.testing-privates.privates-for-testable-privates])
(midje.util/testable-privates as-documentation.testing-privates.privates-for-testable-privates
                              tp-function-without-prerequisite tp-called tp-caller)

(fact "making privates testable by interning them"
  (fact "allows direct testing"
    (tp-function-without-prerequisite 1) => 2)

  (after-silently
   (fact "DOES NOT works with `provided`"
     (tp-caller 1) => 8988
     (provided
       (tp-called 1) => 8988))
   (fact (first @reported) => a-validation-error)))
  

