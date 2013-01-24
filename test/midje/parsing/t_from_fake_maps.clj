(ns midje.parsing.t-from-fake-maps
  (:use [midje sweet test-util]
        [midje.parsing.3-from-lexical-maps.from-fake-maps]
        [utilize.seq :only (find-first only)]
        [midje.test-util]
        midje.util
        clojure.pprint)
  (:require [midje.config :as config])
  (:import midje.data.metaconstant.Metaconstant))

(expose-testables midje.data.prerequisite-state)

(tabular
  (facts "the arg matcher maker handles functions specially"
   ((mkfn:arg-matcher ?expected) ?actual) => ?result)
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


(facts "about result suppliers used"
  "returns identity for =>"
  (let [arrow "=>"]
    ((mkfn:result-supplier arrow [1 2 3])) => [1 2 3])
             
  "returns stream for =streams=>"
  (let [supplier (mkfn:result-supplier "=streams=>" [1 2 3])]
    (supplier) => 1
    (supplier) => 2
    (supplier) => 3))


