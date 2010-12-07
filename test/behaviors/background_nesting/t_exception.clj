(ns behaviors.background-nesting.t-exception
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)
  
(against-background [ (middlemost) => "FOO!" ]
  (try 
    (against-background [ (middlemost) => 33 ]
      (fact (middlemost) => 33)
      (throw (Throwable.)))
    (catch Throwable ex))
  (fact (middlemost) => "FOO!"))

