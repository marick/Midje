(ns midje.util.t-unify
  (:require [midje.util.unify :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(fact "unify works with nil bindings"
  (substitute '(?without-binding (?nil-binding (?other-binding)))
         '{?nil-binding nil, ?other-binding 1})
  => '(?without-binding (nil (1))))

(fact "unify works w/ symbols that don't start with '?'"
  (substitute '(without-binding (nil-binding (other-binding)))
         '{nil-binding nil, other-binding 1})
  => '(without-binding (nil (1))))
  

