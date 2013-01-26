(ns midje.parsing.other.t-arglists
  (:use midje.sweet
        midje.test-util
        midje.parsing.other.arglists)
  (:require [midje.emission.levels :as print-levels]))

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
        non-filter-args #(nth % 2)
        filter-function second
        a-fact (fn [metadata] (with-meta '[] metadata))]

    (fact "filter arguments can be separated out"
      (filter-args (separate-filters an-argument-list))
      => (contains #"a regex" "a string" (exactly map?) :a-keyword))

    (fact "other arguments can be found"
      (non-filter-args (separate-filters an-argument-list))
      => ['a-symbol 6])

    (fact "particular arguments can be special-cased as not filters"
      (filter-args (separate-filters an-argument-list (constantly true))) => empty?)

    (fact "filters are converted into a function"
      (fact "keywords check for the truthiness of the key in the metadata"
        (let [prop-filter (filter-function (separate-filters [:property]))]
          (prop-filter (a-fact {:property 'truthy})) => truthy
          (prop-filter (a-fact {:property false})) => falsey
          (prop-filter (a-fact {})) => falsey))

      (fact "regexes check the fact's name property"
        (let [prop-filter (filter-function (separate-filters [#"regex"]))]
          (prop-filter (a-fact {:midje/name "something containing regex."})) => truthy
          (prop-filter (a-fact {:midje/name "not a match"})) => falsey
          (prop-filter (a-fact {})) => falsey))

      (fact "strings are treated as substrings"
        (let [prop-filter (filter-function (separate-filters ["str"]))]
          (prop-filter (a-fact {:midje/name "something str like"})) => truthy
          (prop-filter (a-fact {:midje/name "not a match"})) => falsey
          (prop-filter (a-fact {})) => falsey))

      (fact "functions are applied to arguments"
        (let [prop-filter (filter-function (separate-filters
                                            [(fn [meta] (= "yes" (:something meta)))]))]
          (prop-filter (a-fact {:something "yes"})) => truthy
          (prop-filter (a-fact {:something "no"})) => falsey
          (prop-filter (a-fact {})) => falsey))

      (fact "default judgment is true"
        (let [prop-filter (filter-function (separate-filters []))]
          (prop-filter (a-fact {})) => truthy))

      (fact "multiple arguments are OR'd together"
         (let [prop-filter (filter-function (separate-filters [#"foo" :valiant]))]
           (prop-filter (a-fact {:midje/name "ofoop"})) => truthy
           (prop-filter (a-fact {:valiant true})) => truthy
           (prop-filter (a-fact {})) => falsey))

      (fact "filter predicates know why they were created"
        (:created-from (meta (filter-function (separate-filters [:oddity :valiant]))))
        => [:oddity :valiant])

  )))      
      


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





