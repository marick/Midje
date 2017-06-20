(ns implementation.parsing.util.fim_recognizing
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.parsing.2-to-lexical-maps.expects :refer [expect]]
            [midje.parsing.util.recognizing :refer :all]
            [clojure.zip :as zip]))

;;; Arrows

(tabular
 (fact "an embedded expect form can be recognized"
   (expect? (zip/seq-zip ?form)) => ?expected)

 ?form                                  ?expected
 `(expect x => y)                       truthy
 '(+ x y)                               falsey
 'expect                                falsey)


(fact "some arrows expect matches, some mismatches"
  (let [result (map expect-match-or-mismatch
                    '(=> midje.data.core-maps/=> midje.sweet/=>
                      =not=> midje.data.core-maps/=not=> midje.sweet/=not=>
                      =expands-to=> midje.sweet/=expands-to=>))]
    (fact result => [:expect-match :expect-match :expect-match
                     :expect-mismatch :expect-mismatch :expect-mismatch
                     :expect-match :expect-match])))


(fact "can ask if at first element of X =?> Y :possible :keywords"
  (let [possible (fn [nested-form] (zip/down (zip/seq-zip nested-form)))]
              "a string" =not=> start-of-checking-arrow-sequence?
              '(foo) =not=> start-of-checking-arrow-sequence?

              '( (f 1) ) =not=> start-of-checking-arrow-sequence?
    (possible '( (f 1) )) =not=> start-of-checking-arrow-sequence?

              '( (f 1) (f 2)) =not=> start-of-checking-arrow-sequence?
    (possible '( (f 1) (f 2))) =not=> start-of-checking-arrow-sequence?

              '( (f 1) => 2) => start-of-checking-arrow-sequence?
    (possible '( (f 1) => 2)) => start-of-checking-arrow-sequence?

              '( (f 1) =not=> 2) => start-of-checking-arrow-sequence?
    (possible '( (f 1) =not=> 2)) => start-of-checking-arrow-sequence?

              '( (f 1) => 2 :key 'value) => start-of-checking-arrow-sequence?
    (possible '( (f 1) => 2 :key 'value)) => start-of-checking-arrow-sequence?

              '( (f 1) midje.sweet/=> 2) => start-of-checking-arrow-sequence?
              (possible '( (f 1) midje.sweet/=> 2)) => start-of-checking-arrow-sequence?))

(fact "some of the arrow forms for prerequisites differ"
  '( (f) => 3) => start-of-checking-arrow-sequence?  ; Not this one
  '( (rand) =streams=> [1 2 3]) => start-of-prerequisite-arrow-sequence?
  '( (f) =not=> empty?) =not=> start-of-prerequisite-arrow-sequence?
  '((before :facts (f))) =not=> start-of-prerequisite-arrow-sequence?)

;;; Provided

(fact "can ask whether at the beginning of a form that provides prerequisites"
  (let [values (zip/seq-zip '(provided midje.sweet/provided fluke))]
    (-> values zip/down) => provided?
    (-> values zip/down zip/right) => provided?
    (-> values zip/down zip/right zip/right) =not=> provided?))



