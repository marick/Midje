(ns as-documentation.about-defrecord.using-refer--provided-tests.specific
  (:require [midje.sweet :refer :all])
  (:require [midje.open-protocols :refer [defrecord-openly]])   ; again, we use `defrecord-openly`
  (:require [as-documentation.about-defrecord.generic
             :refer [Numerical
                     bump ; required because of self-call of bump.
                     twice ; required only so that the test later can refer to it
                     ]]))


(defrecord-openly Record [n]
  Numerical
  (bump [_ by] (+ n by))
  (bump [this] (bump this 1))           ; note self-call
  (twice [this] (bump this (:n this))))

(facts "tests of the record work as before"
  (let [rec (->Record 3)]
    (bump rec) => 4
    (bump rec 5) => 8
    (twice rec) => 6))

(fact "you can now mock out one of these methods"
  (let [rec (->Record 3)]
    (+ 1 (bump rec)) => 0
    (provided
      (bump rec) => -1)))

(fact "you can use prerequisites to define one protocol method in terms of another"
  (let [rec (->Record 3)]
    (twice rec) => ..bogus..
    (provided
      (bump rec 3) => ..bogus..)))


