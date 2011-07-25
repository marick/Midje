;; -*- indent-tabs-mode: nil -*-

(ns midje.t-fakes
  (:use [midje fakes sweet test-util]))


(tabular
 (facts "the arg matcher maker hadles functions specially"
   ((arg-matcher-maker ?expected) ?actual) => ?result)
 ?expected              ?actual         ?result
 1                      1               TRUTHY
 1                      odd?            falsey

 anything               3               TRUTHY
 anything               odd?            TRUTHY
 (roughly 3)            3               TRUTHY
 (roughly 3)            0               falsey
 (contains 1)           [3 1]           TRUTHY
 (contains 1)           [3 3]           falsey
 (contains odd?)        [3]             TRUTHY
 (contains odd?)        [2 odd?]        falsey

 (exactly odd?)         odd?            TRUTHY
 (exactly odd?)         3               falsey

 (as-checker odd?)      odd?            falsey
 (as-checker odd?)      3               TRUTHY

 odd?                   odd?            TRUTHY
 odd?                   3               falsey)

 

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


         

(defn called-because-mock-checking-requires-it [] nil)
(defn has-faked-function []
  (called-because-mock-checking-requires-it)
  (function-tagged-as-fake? called-because-mock-checking-requires-it))
         
(fact "A faked function can be identified from its metadata"
  (has-faked-function) => falsey
  (has-faked-function) => truthy
  (provided
    (called-because-mock-checking-requires-it) => 33))
  
(fact "make-fake's result has the line number of the arrow form"
  (let [args `( ~(at-line 789 '(f 1)) => 3)]
    (:line (meta (make-fake args))) => 789))

     
