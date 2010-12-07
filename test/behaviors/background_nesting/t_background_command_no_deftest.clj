(ns behaviors.background_nesting.t-background-command-no-deftest
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint)
)

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(background (outermost ...o...) => 1)
(fact (+ 1 (outermost ...o...)) => 2)
(background (outermost ...o...) => -1)
(fact (+ 1 (outermost ...o...)) => 0)

