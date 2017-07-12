;; To override prerequisite functions defined in protocols, we have to define the record
;; specially.

(ns as-documentation.about-defrecord.using-as--provided-tests.specific
  (:require [midje.sweet :refer :all]
            [midje.open-protocols :refer [defrecord-openly]])   ; <= this
  (:require [as-documentation.about-defrecord.generic :as generic]))

;; In contrast to the `defrecord` case (in `using_as__plain_tests/specific.clj`), the
;; fact that the var-having-a-function-associated-with-it belongs to another package
;; must be made explicit. That is, we could there do:
;;   (bump [_ by] (+ n by))
;; ... but here we must do
;;   (generic/bump [_ by] (+ n by))
;; This is unexpected enough that I consider it a bug. (Though you could argue that
;; its `defrecord`'s special-casing that's the original sin.)

;; However, such variation from `defrecord` is not required in the
;; perhaps more idiomatic `refer` case.
;; (as-documentation.about-defrecord.using-refer--provided-tests.specific)

(defrecord-openly Record [n]
  generic/Numerical
  (generic/bump [_ by] (+ n by))
  (generic/bump [this] (generic/bump this 1))
  (generic/twice [this] (generic/bump this (:n this))))

(fact "tests of the record work as in the `defrecord` case"
  (let [rec (->Record 3)]
    (generic/bump rec) => 4
    (generic/bump rec 5) => 8
    (generic/twice rec) => 6))

(fact "and you can now mock out one of these methods"
  (let [rec (->Record 3)]
    (+ 1 (generic/bump rec)) => 0
    (provided
      (generic/bump rec) => -1)))

(fact "and you can use prerequisites to define one protocol method in terms of another"
  (let [rec (->Record 3)]
    (generic/twice rec) => ..bogus..
    (provided
      (generic/bump rec 3) => ..bogus..)))
