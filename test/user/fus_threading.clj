(ns user.fus-threading
  (:use midje.sweet
        midje.test-util
        midje.util.ecosystem))
  
(when-1-4+

 (use 'plumbing.core)
 (require '[plumbing.graph :as graph])

 (defn tak-y [_] 5)

 (defn tak [x y z]
   (if (< y x)
     (tak (tak (dec x) (tak-y 4) z)
          (tak (dec y) (tak-y 4) x)
          (tak (dec z) (tak-y 4) y))
     z))
 
 (def computation-tree 
   {:one (fnk [arg] (tak arg (tak-y 4) 20))
    :two (fnk [arg] (tak arg (tak-y 4) 20))
    :three (fnk [arg] (tak arg (tak-y 4) 20))
    :all (fnk [one two three] [one two three])})
 
 (def computation (graph/par-compile computation-tree))
 
 (fact "prerequisite bindings persist into other threads"
   (:all (computation {:arg 20})) => [4 4 4]
   (provided (tak-y 4) => 4))
 
) ; 1.4+

  
