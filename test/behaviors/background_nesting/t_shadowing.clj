(ns behaviors.background-nesting.t-shadowing
  (:require [clojure.test :refer :all]
            [midje
             [sweet :refer :all]
             [test-util :refer :all]]))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)


(deftest background-command-is-shadowed-by-against-background  ; deftest intentional
  (background (outermost) => 2
              (middlemost) => 'a)
  (against-background [ (middlemost) => 33]
    (fact (+ (middlemost) (outermost)) => 35)))
  
