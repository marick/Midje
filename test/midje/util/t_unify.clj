;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-unify
  (:use midje.util.unify
        midje.sweet
        midje.test-util))

(fact "unify works with nil bindings"
  (subst '(?without-binding (?nil-binding (?other-binding)))
         '{?nil-binding nil, ?other-binding 1})
  => '(?without-binding (nil (1))))
  

(fact "There can be missing arguments"
  (bindings-map-or-nil '(required variable) '(required ?var ?required2 ?variable2))
                                          => '{?var variable ?required2 nil ?variable2 nil}

  (bindings-map-or-nil '(required variable required var2)
                       '(required ?var ?required2 ?variable2))
                    => '{?var variable ?required2 required ?variable2 var2})
                                          

