(ns midje.parsing.other.t-arglists
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.parsing.other.arglists :refer :all]
            [midje.emission.levels :as print-levels]))

(facts "separating levels out of argument lists"
  (separate-print-levels [] :print-normally)
  => [ [] :print-normally [] ]

  (separate-print-levels [:print-nothing] ..irrelevant..)
  => [[:print-nothing] :print-nothing []]

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

(def a-body '((f) => 3))


(fact "separating metadata filters out of argument lists"
  (let [an-argument-list [#"a regex" "a string" 'a-symbol 6 map? :a-keyword]

        filter-args first
        non-filter-args #(peek %)
        a-fact (fn [metadata] (with-meta '[] metadata))

        no-special-args (constantly false)
        all-args-are-special (constantly true)]

    (fact "filter arguments can be separated out"
      (filter-args (separate-filters an-argument-list no-special-args))
      => (contains #"a regex" "a string" (exactly map?) :a-keyword))

    (fact "other arguments can be found"
      (non-filter-args (separate-filters an-argument-list no-special-args))
      => ['a-symbol 6])

    (fact "a function can be used to mark arguments that would ordinarily be
           filters as non-filters"
      (separate-filters an-argument-list all-args-are-special)
      => (just [] an-argument-list))))




(fact "arglist parser with :options"
  (let [flag-descriptions [[:dirs :dir] [:interval]]
        parser (apply make-option-arglist-parser flag-descriptions)]

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





