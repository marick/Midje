(ns midje.parsing.3-from-lexical-maps.t-from-fake-maps
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.parsing.3-from-lexical-maps.from-fake-maps :refer :all]
            [midje.test-util :refer :all]
            [midje.util :refer :all]
            [midje.config :as config]))

(expose-testables midje.data.prerequisite-state)

(tabular
  (facts "the arg matcher maker handles functions specially"
   ((apply mkfn:arglist-matcher-fixed-arity       [?expected]) [?actual]) => ?result)
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

(fact "sometimes an arglist must be matched exactly"
  ((mkfn:arglist-matcher-fixed-arity 1 2) [1    ]) => falsey
  ((mkfn:arglist-matcher-fixed-arity 1 2) [1 2  ]) => truthy
  ((mkfn:arglist-matcher-fixed-arity 1 2) [1 2 3]) => falsey)

(fact "an arglist can allow rest args"
  ((mkfn:arglist-matcher-allowing-optional-args 1 2 & anything) [1    ]) => falsey
  ((mkfn:arglist-matcher-allowing-optional-args 1 2 & anything) [1 2  ]) => truthy
  ((mkfn:arglist-matcher-allowing-optional-args 1 2 & anything) [1 2 3]) => truthy

  (fact "the required args are treated the same as the fixed-arity case"
    ( (mkfn:arglist-matcher-allowing-optional-args 1 even?              & anything) [1 2 3]) => falsey
    ( (mkfn:arglist-matcher-allowing-optional-args 1 (as-checker even?) & anything) [1 2 3]) => truthy)

  (fact "the argument after the & is treated as a checker"
    ((mkfn:arglist-matcher-allowing-optional-args 1 2 & (as-checker empty?)) [1 2]) => truthy
    ((mkfn:arglist-matcher-allowing-optional-args 1 2 &             empty? ) [1 2]) => falsey
    ((mkfn:arglist-matcher-allowing-optional-args 1 2 & (as-checker empty?)) [1 2 3]) => falsey))

(facts "about result suppliers used"
  "returns identity for =>"
  (let [arrow "=>"]
    ((mkfn:result-supplier arrow (fn [] [1 2 3]))) => [1 2 3])

  "returns stream for =streams=>"
  (let [supplier (mkfn:result-supplier "=streams=>" (fn [] [1 2 3]))]
    (supplier) => 1
    (supplier) => 2
    (supplier) => 3))


