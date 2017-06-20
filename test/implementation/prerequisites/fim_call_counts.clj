(ns implementation.prerequisites.fim_call_counts
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.checking.checkables :refer :all]))

(tabular
  (fact "The expected call count can be described"
    (let [prerequisite-description
          {:call-count-atom (atom ?actual-count),
           :type ?type
           :times ?expected-count}]
      (call-count-incorrect? prerequisite-description) => ?matches))

  ?type          ?actual-count   ?expected-count        ?matches
  :fake          0               :default                TRUTHY
  :fake          1               :default                falsey
  :fake          200             :default                falsey

  :fake          2              (range 0 2)              TRUTHY
  :fake          1              (range 0 2)              falsey
  :fake          1              #{0 2}                   TRUTHY

  :fake          1              2                        TRUTHY
  :fake          1              1                        falsey

  :fake          1               even?                   TRUTHY
  :fake          2               even?                   falsey)


