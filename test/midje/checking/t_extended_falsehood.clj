(ns midje.checking.t-extended-falsehood
  (:use midje.sweet
        [midje.checking.extended-falsehood]
        midje.test-util
        clojure.pprint))

(facts "about an extended notion of falsehood"
  (extended-false? false) => truthy
  (extended-false? true) => falsey
  (extended-false? {:intermediate-results 3}) => falsey
  (extended-false? (as-data-laden-falsehood {})) => truthy

  ".. and its inverse"
  (extended-true? false) => falsey
  (extended-true? true) => truthy
  (extended-true? {:intermediate-results 3}) => truthy
  (extended-true? (as-data-laden-falsehood {})) => falsey)

(facts "about data-laden falsehoods"
  (as-data-laden-falsehood [5]) => data-laden-falsehood?
  (meta (as-data-laden-falsehood (with-meta [5] {:foo true}))) => (contains {:foo true}))
  
(facts "user-friendly-falsehood converts extended-falsehood into just false"
  (user-friendly-falsehood false) => false
  (user-friendly-falsehood nil) => nil
  (user-friendly-falsehood (as-data-laden-falsehood {})) => false)

  
  
