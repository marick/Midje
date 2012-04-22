(ns midje.checkers.t-extended-falsehood
  (:use midje.sweet
        [midje.checkers.extended-falsehood :only [extended-false? as-data-laden-falsehood
                                                  data-laden-falsehood?]]
        midje.test-util
        clojure.pprint))

(facts "about an extended notion of falsehood"
  (extended-false? false) => truthy
  (extended-false? true) => falsey
  (extended-false? {:intermediate-results 3}) => falsey
  (extended-false? (as-data-laden-falsehood {})) => truthy)

(facts "about data-laden falsehoods"
  (as-data-laden-falsehood [5]) => data-laden-falsehood?
  (meta (as-data-laden-falsehood (with-meta [5] {:foo true}))) => (contains {:foo true}))