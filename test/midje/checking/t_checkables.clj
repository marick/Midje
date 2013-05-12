(ns midje.checking.t-checkables
  (:use [midje sweet test-util]
        midje.checking.checkables))

(tabular (fact "The number of calls can be described"
           (let [fake-fake {:call-count-atom (atom ?actual-count),
                            :type ?type
                            :times ?specified-count}]
           (call-count-incorrect? fake-fake) => ?expected))

         ?type          ?actual-count   ?specified-count        ?expected
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


