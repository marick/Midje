;; -*- indent-tabs-mode: nil -*-

(ns midje.t-prerequisites
  (:use [midje.prerequisites])
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.semi-sweet/provided fluke))]
    (-> values zip/down) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right) => is-head-of-form-providing-prerequisites?
    (-> values zip/down zip/right zip/right) =not=> is-head-of-form-providing-prerequisites?))


;; Folded prerequisites

(defmacro some-macro [& rest] )


(tabular 
 (fact "things that are not fake-sexps don't need to be unfolded"
   ?thing ?arrow folded-prerequisite?)

  ;; things not a proper fake macro
  ?thing                                        ?arrow
  '1                                            =not=> 
  '()                                           =not=> 
  '(fake (f (h 1)))                             =not=> ; not in right namespace
  '(midje.semi-sweet/non-fake (f (h 1)))        =not=>

  ;; Sad but true: a cons is not a list.
  (cons 'midje.semi-sweet/fake '((f (h 3)) =test=> 3))    => )

(tabular
 (fact "unfolding depends on the inner structure of a funcall"
  '(midje.semi-sweet/fake ?call =test=> 3) ?arrow folded-prerequisite?)
   
 ?call                  ?arrow
 ;; Things that might be misinterpreted as nested funcalls
  (f)                  =not=> 
  (f 1)                =not=> 
  (f 1 '(foo))         =not=> 
  (f 1 [foo])          =not=> 
  (f 1 {foo 1})        =not=> 

  ;; These are real nested function calls
  (f (h 1))              => 
  (f 1 (h 1))            => 

  ;; but don't decide to unfold a checker used as argument matcher"
  (f 1 (exactly even?))  =not=>

  ;; don't unfold a constructor.
  (f (java.util.Date. 1 1 1))           =not=>
  (f (new java.util.Date 1 2 2))        =not=>
  
  "Macros are surprisingly hard to get right"
;  '(f 1 (some-macro 33))  =not=> folded-prerequisite?
  )
