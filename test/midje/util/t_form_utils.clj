(ns midje.util.t-form-utils
  (:use [midje.util.form-utils])
  (:use [midje.sweet])
  (:use [midje.test-util]))

(defrecord R [a])

(facts "about recognizing classic maps"
  (classic-map? {}) => truthy
  (classic-map? (R. 1)) => falsey
  (classic-map? 1) => falsey)

(facts "about recognizing records"
  (record? {}) => falsey
  (record? (R. 1)) => truthy
  (record? 1) => falsey)

(facts "a form's reader-assigned line-number can be extracted"
  (reader-line-number (with-meta '(fact (this that)) {:line 23})) => 23
  "or, failing that: try top-level subforms"
  (reader-line-number `(fact
                         (+ 1 2)
                         ~(with-meta '(this that) {:line 23})
                         ~(with-meta '(this that) {:line 22223}))) => 23
  "or a default value"
  (reader-line-number (with-meta '(fact "text") {})) => "0 (no line info)")

(facts "extract elements from vectors and return remainder"
  (vector-without-element-at-index 0 [0 1 2]) => vector?
  (vector-without-element-at-index 0 [0 1 2]) => [1 2]
  (vector-without-element-at-index 1 [0 1 2]) => [0 2]
  (vector-without-element-at-index 2 [0 1 2]) => [0 1])

(facts "you can tack new keys onto a hashmap"
  (tack-on-to {:a [1], :b [55] :c 'blah} :a 2 :b 56) => {:a [1 2], :b [55 56], :c 'blah})

(fact "it can be useful to get hash-map to allow duplicates"
  (hash-map-duplicates-ok) => {} 
  (hash-map-duplicates-ok :a 1 :b 2) => {:a 1 :b 2}
  (hash-map-duplicates-ok :a 1 :b 2 :b 33333) => {:a 1 :b 33333})

(fact "pairs are exciting"
  (pairs [:a :b :c] [1 2 3]) => [ [:a 1] [:b 2] [:c 3] ])

(fact "map-difference"
  (map-difference {:a 1, :b 2} {:a 1, :c 3}) => {:b 2})

(fact "apply each function to each corresponding arg" 
  (apply-pairwise [inc dec] [1 1] [2 2]) => [[2 0] [3 1]])

(fact "divide seq into two seqs: items that pass a predicate and a items the fail"
  (separate-by odd? [1 2 3]) => [ [1 3] [2] ]
  "works for predicates that don't return true or false"
  (separate-by #(if (odd? %) nil %) [1 2 3]) => [[2] [1 3]])

(fact "checks each pred against the result of the first expression, returning if it finds a match" 

  (pred-cond "abcde" 
    #(.contains % "xyz") "contains 'xyz'" 
    string? "string"
    :else "neither") => "string"

  (pred-cond 1 
    even? "even" 
    string? "string"
    :else "neither") => "neither"
  
  "Don't need an :else"
  (pred-cond 1 
    even? "even") => nil)

(fact 
  (map-first str [1 2 3]) => ["1" 2 3])

(tabular
  (fact "A single argument can be converted into a structured-form and a arg-value-name"
    (against-background (unique-argument-name) => 'unique-3)
    (let [[form name] (single-arg-into-form-and-name ?original)]
      form => ?form
      name => ?name))
  ?original              ?form                       ?name
  'a                     'a                          'a
  '[a b]                 '[a b :as unique-3]         'unique-3
  '[a b & c :as all]     '[a b & c :as all]          'all
  '{:keys [a b]}         '{:keys [a b] :as unique-3} 'unique-3
  '{:keys [a b] :as all} '{:keys [a b] :as all}      'all
  ;; pathological cases
  '[a]                   '[a :as unique-3]           'unique-3
  '[a :as b]             '[a :as b]                  'b)


(defrecord ExampleNamed []
  clojure.lang.Named
  (getName [this] "name")
  (getNamespace [this] "namespace"))

(tabular
  (fact "can tell if a thing is Named"
    (named? ?thing) => ?result)
  
  ?thing           ?result
  'a               truthy
  :keyword         truthy
  (ExampleNamed.)  truthy
  "a"              falsey
  1                falsey
  \c               falsey)

(fact "sort a map"
  (sort-map {:z 26 :b 2 :a 1}) => {:a 1 :b 2 :z 26})
