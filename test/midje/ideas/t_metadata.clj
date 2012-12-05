(ns midje.ideas.t-metadata
  (:use midje.ideas.metadata
        midje.sweet midje.test-util)
  (:require [midje.config :as config]))

(def a-body '((f) => 3))

(facts "about separate-metadata" 
  (fact "contains the original source and other info"
    (let [[meta _] (separate-metadata `(fact "doc" ~@a-body))]
      (:midje/source meta) => `(fact "doc" ~@a-body)
      (:midje/file meta) => "midje/ideas/t_metadata.clj"
      (:midje/namespace meta) => 'midje.ideas.t-metadata
      (contains? meta :midje/line) => truthy))

  
  (fact "ignores the head of the form"
    (let [[meta _] (separate-metadata `(FOO "doc" ~@a-body))]
      (:midje/source meta) => `(FOO "doc" ~@a-body)
      (:midje/name meta) => "doc"))
  
  (fact "doc strings"
    (fact "can be separated"
      (let [[meta body] (separate-metadata `(fact "doc" ~@a-body))]
        (:midje/description meta) => "doc"
        body => a-body))
    (fact "need not be present"
      (let [[meta body] (separate-metadata `(fact ~@a-body))]
        (:midje/description meta) => nil
        body => a-body))
    (fact "can provide the name"
      (let [[meta body] (separate-metadata `(fact "doc" ~@a-body))]
        (:midje/name meta) => "doc"
        body => a-body)))
  
  (facts "symbols"
    (fact "become the fact name"
      (let [[meta body] (separate-metadata `(fact cons ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body))
    (fact "take precedence over strings"
      (let [[meta body] (separate-metadata `(fact "foo" cons ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body)
      (let [[meta body] (separate-metadata `(fact cons "foo" ~@a-body))]
        (:midje/name meta) => "cons"
        body => a-body))
    (fact "don't count as names when they are the head of an expect form"
      (let [[meta body] (separate-metadata `(fact foo => 3))]
        (:midje/name meta) => nil
        body => `(foo => 3))))

  (fact "keywords become true metadata"
    (let [[meta body] (separate-metadata `(fact :a :b  ~@a-body))]
      (:a meta) => true
      (:b meta) => true
      body => a-body))

  (fact "metadata can be an explicit map"
    (let [[meta body] (separate-metadata `(fact name {:a 1}  ~@a-body))]
      (:midje/name meta) => "name"
      (:a meta) => 1
      body => a-body)))

(fact "metadata can be promoted from a nested form"
  (let [form '(tabular (fact ?a => 2) ?a 1)]
    (promote-metadata form) => form)
  (let [form '(tabular :a (fact ?a => 2) ?a 1)]
    (promote-metadata form) => form)
  (let [form '(tabular :a (fact :b ?a => 2) ?a 1)]
    (promote-metadata form) => form)

  (let [form '(tabular (fact :b ?a => 2) ?a 1)
        promoted (promote-metadata form)]
    (nth promoted 0) => 'tabular

    (nth promoted 1) => map?
    (let [metadata (nth promoted 1)]
      (:b metadata) => true
      (:midje/source metadata) => '(tabular (fact :b ?a => 2) ?a 1)
      (:midje/file metadata) => "midje/ideas/t_metadata.clj"
      (:midje/namespace metadata) => 'midje.ideas.t-metadata
      (contains? metadata :midje/line) => truthy)

    (nth promoted 2) => '(fact ?a => 2)
    (drop 3 promoted) => '(?a 1)))

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


         
