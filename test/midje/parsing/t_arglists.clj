(ns midje.parsing.t-arglists
  (:use midje.sweet
        midje.test-util
        midje.parsing.arglists)
  (:require [midje.emission.levels :as print-levels]))

(facts "separating levels out of argument lists"
  (separate-print-levels [] :print-normally)
  => [ [] :print-normally nil ]

  (separate-print-levels [:print-nothing] ..irrelevant..)
  => [[:print-nothing] :print-nothing nil]

  (separate-print-levels [:all :print-nothing] ..irrelevant..)
  => [[:print-nothing] :print-nothing [:all]]

  (separate-print-levels [:print-nothing :all] ..irrelevant..)
  => [[:print-nothing] :print-nothing [:all]]
  
  (let [number-form (print-levels/names-to-levels :print-facts)]
    (separate-print-levels ['a number-form 'b] ..irrelevant..)
    => [[number-form] number-form '[a b]])
  
  ;; checks for valid levels
  (separate-print-levels [500 'something] ..irrelevant..)
  => (throws Error #"500.*not.*valid")

  (separate-print-levels [:print-nothing :print-facts] ..irrelevant..)
  => (throws Error #"extra.*print.*level"))



(fact "Segmentizing arglist parser"
  (let [flag-descriptions [[:dirs :dir] [:interval]]
        parser (apply make-segmentingizing-arglist-parser flag-descriptions)]
     
    (fact "can find true args"
    (parser []) => (contains {:true-args []})
    (parser [:dirs]) => (contains {:true-args []}))
      
    (fact "can find flags that are present"
      (parser ['arg :dirs 1 2]) => (contains '{:dirs? true :dirs-args [1 2]}))

    (fact "can find flags that are absent"
      (parser [:dirs 1 2]) => (contains '{:interval? false}))

    (fact "flag synonyms are equivalent to the first argument in the description"
      (parser [:dir 1 2]) => (contains '{:dirs? true :dirs-args [1 2]}))

    (fact "putting it all together"
      (parser []) => {:true-args []
                      :dirs? false
                      :interval? false}

      (parser [1]) => {:true-args [1]
                      :dirs? false
                      :interval? false}

      (parser [:dirs]) => {:true-args []
                            :dirs? true
                            :dirs-args []
                            :interval? false}

      (parser [:dir 1]) => {:true-args []
                            :dirs? true
                            :dirs-args [1]
                            :interval? false}

      (parser [1 :interval 2 3 :dir 4]) => {:true-args [1]
                                            :dirs? true
                                            :dirs-args [4]
                                            :interval? true
                                            :interval-args [2 3]})))
