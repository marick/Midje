(ns midje.ideas.t-metadata
  (:use midje.ideas.metadata
        midje.sweet midje.test-util)
  (:require [midje.config :as config]))

(def a-body '((f) => 3))




(tabular "fact-body-source converts a fact function into its body (ignoring metadata)"
  (fact
    (fact-body-source (with-meta ["faux fact"] {:midje/source ?full-body}))
    => ?expected)
  ?full-body                          ?expected
  '(fact "name" 1 => 2)               '(1 => 2)
  '(fact [1] => 2)                    '([1] => 2)
  '(fact {:a 1 :b 2} "name" (dorm))   '((dorm)))


;;; Predicate constructors


(fact "filter predicates"
  (letfn [(b [& kvs] (with-meta a-body (apply hash-map kvs)))]
    (fact "single entries"
      ((desired-fact-predicate-from ["foo"]) (b :midje/name "ofoop")) => true
      ((desired-fact-predicate-from [#"foo"])  (b :midje/name "ofoop")) => true
      ((desired-fact-predicate-from [#"foo"])  (b                    )) => false

      ((desired-fact-predicate-from [:valiant]) (b :valiant "yes!")) => true
      ((desired-fact-predicate-from [:valiant]) (b                )) => false
      ((desired-fact-predicate-from [#(= "yes!" (:valiant %))]) (b :valiant "yes!")) => true
      ((desired-fact-predicate-from [#(= "yes!" (:valiant %))]) (b :valiant "nope")) => false)

  (fact "multiple entries act as 'or'"
    (let [combo (desired-fact-predicate-from [#"foo" :valiant])]
      (combo (b :midje/name "ofoop")) => true
      (combo (b :valiant true)) => true
      (combo (b              )) => false))

  (fact "have information about how they were created"
    (:created-from (meta (desired-fact-predicate-from [:oddity :valiant])))
    => [:oddity :valiant])))


(fact "it knows how to separate out metadata-filtering arguments"
  (let [[filter filter-function remainder]
        (separate-filters [#"a regex" "a string" 'a-symbol 6 map? :a-keyword]
                          (fn plain-argument?? [arg] false))]
    filter => (contains #"a regex" "a string" (exactly map?) :a-keyword)
    (filter-function (with-meta (fn[]) {:midje/name "matches a regex"})) => true
    (filter-function (with-meta (fn[]) {:midje/name "matches a string"})) => true
    (filter-function (with-meta (fn[]) {})) => true
    (filter-function (with-meta (fn[]) {:a-keyword 'yes})) => true
    remainder => ['a-symbol 6]))


         
