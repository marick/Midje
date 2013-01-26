(ns midje.util.t-form-utils
  (:use [midje.util.form-utils])
  (:use [midje.sweet])
  (:use [midje.test-util]))

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

(fact "pairs are exciting"
  (pairs [:a :b :c] [1 2 3]) => [ [:a 1] [:b 2] [:c 3] ])

(fact "map-difference"
  (map-difference {:a 1, :b 2} {:a 1, :c 3}) => {:b 2})

  
(fact "apply each function to each corresponding arg" 
  (apply-pairwise [inc dec] [1 1] [2 2]) => [[2 0] [3 1]])

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

(tabular
  (fact "A single argument can be converted into a structured-form and a arg-value-name"
    (against-background (gensym 'symbol-for-destructured-arg) => 'unique-3)
    (let [[form name] (single-destructuring-arg->form+name ?original)]
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


(fact "sort a map"
  (sort-map {:z 26 :b 2 :a 1}) => {:a 1 :b 2 :z 26})


(fact "any-pred-from"
  ((any-pred-from [odd? even?]) 1) => true
  ((any-pred-from [pos? neg?]) 0) => false
  ((any-pred-from [:key :word]) {:key false}) => false
  ((any-pred-from [:key :word]) {:key false :word 3}) => true
  ((any-pred-from [#{1 2} #{3 4}]) 3) => true
  ;; stops at first match
  ((any-pred-from [(partial = 3) (fn[_](throw (new Error "boom!")))]) 3) => true
  ;; Any empty list means that everything matches
  ((any-pred-from []) 3) => true)

(fact "stringlike-matches?"
  (stringlike-matches? "foo" "ofoop") => true
  (stringlike-matches? "foo" "ooop") => false
  (stringlike-matches? "foo" nil) => false
  (stringlike-matches? "foo" [1 2 3]) => false
  (stringlike-matches? #"fo." "ofop") => true
  (stringlike-matches? #"fo." "ooop") => false
  (stringlike-matches? #"fo." false) => false)
    

  
(fact "can unquote a form"
  (dequote '1) => 1
  (dequote 1) => 1
  (dequote '(some form)) => '(some form))

