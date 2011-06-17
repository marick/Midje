;; -*- indent-tabs-mode: nil -*-

(ns midje.t-fakes
  (:use [midje fakes sweet test-util]))

(declare f g)
(fact "unique variables can be found in fakes"
  (let [fakes [ (fake (f 1) => 2)
                (fake (f 2) => 4)
                (fake (g) => 3)] ]
    (unique-function-vars fakes) => (contains [#'f #'g] :in-any-order)))

(tabular
 (facts "matching calls depend on both function name and arguments"
   (let [fake {:function 'expected, :arg-matchers [ odd? ] }]
     (find-matching-call ?faked-fun ?args [fake]) ?arrow fake))

 ?faked-fun     ?args   ?arrow
 'not-expected  [3]     =not=>  ; function name
 'expected      [3 3]   =not=>  ; arg count
 'expected      [4]     =not=>  ; arg value
 'expected      [3]     =>)

(fact "fakes keep track of their call counts"
  (let [fakes [(fake (f 1) => 3)
               (fake (g 1) => 4)
               (fake (f 2) => 5)]
        counts #(map fake-count fakes)]
    (call-faker #'f [1] fakes)    (counts) => [1 0 0]
    (call-faker #'f [1] fakes)    (counts) => [2 0 0]
    (call-faker #'f [2] fakes)    (counts) => [2 0 1]
    (call-faker #'g [1] fakes)    (counts) => [2 1 1]))

(fact "Unintuitively, earlier binding maps override later"
  (let [fakes [(fake (f 1) => 3 :type :background)
               (fake (f 1) => 4 :type :background)]
        result-map (binding-map fakes)]

    (call-faker (var f) [1] fakes)
    (map fake-count fakes) => [1 0]))




(tabular (fact "The number of calls can be described"
           (let [fake-fake {:count-atom (atom ?actual-count),
                            :type ?type
                            :times ?specified-count}]
           (call-count-incorrect? fake-fake) => ?expected))

         ?type          ?actual-count   ?specified-count        ?expected
         :fake          0               nil                     TRUTHY
         :fake          1               nil                     falsey
         :fake          200             nil                     falsey

         :fake          2              (range 0 2)              TRUTHY
         :fake          1              (range 0 2)              falsey
         :fake          1              #{0 2}                   TRUTHY

         :fake          1              2                        TRUTHY
         :fake          1              1                        falsey

         :fake          1               even?                   TRUTHY
         :fake          2               even?                   falsey)


         

         
         
