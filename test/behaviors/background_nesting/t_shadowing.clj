(ns behaviors.background-nesting.t-shadowing
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)


(deftest background-command-is-shadowed-by-against-background
  (background (outermost) => 2
	      (middlemost) => 'a)
  (against-background [ (middlemost) => 33]
    (fact (+ (middlemost) (outermost)) => 35)))
  
