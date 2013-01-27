(ns midje.clojure.t-core
  ;; (:use midje.clojure.core) - note that test-util immigrates midje.clojure.core
  (:use midje.sweet
        midje.test-util))

;;; Types and pseudo types

(defrecord R [a])

(facts "about recognizing classic maps"
  (classic-map? {}) => truthy
  (classic-map? (R. 1)) => falsey
  (classic-map? 1) => falsey)

(facts "about recognizing records"
  (record? {}) => falsey
  (record? (R. 1)) => truthy
  (record? 1) => falsey)

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


;;; Vars

(def #^:dynamic var-with-root :original-value)
(fact "can get the root value in both Clojure 1.3 and 1.2"
  (var-root #'var-with-root) => :original-value
  (binding [var-with-root "some other value"]
    var-with-root => "some other value"
    (var-root #'var-with-root) => :original-value))

;;; Maps

(fact "it can be useful to get hash-map to allow duplicates"
  (hash-map-duplicates-ok) => {} 
  (hash-map-duplicates-ok :a 1 :b 2) => {:a 1 :b 2}
  (hash-map-duplicates-ok :a 1 :b 2 :b 33333) => {:a 1 :b 33333})

(fact "invert"
  (invert {:a 1, :b 2}) => {1 :a, 2 :b})

(fact "dissoc-keypath"
  (fact "removes a key/value pair"
    (dissoc-keypath {:by-name {:name1 1, :name2 2}} [:by-name :name1])
    =>              {:by-name {          :name2 2}}
    (dissoc-keypath {:by-name {:name1 1}} [:by-name :name1])
    =>              {:by-name {        }}
    (dissoc-keypath {"1" {"2" {"3.1" 3, "3.2" 3}}} ["1" "2" "3.1"])
    =>              {"1" {"2" {         "3.2" 3}}})
  (fact "leaves the map alone if the last key is incorrect"
    (dissoc-keypath {:by-name {:name1 1}} [:by-name :NOTFOUND])
    =>              {:by-name {:name1 1}})
  (fact "requires that the path up to the last key exists"
    (dissoc-keypath {:by-name {:name1 1}} [:NOTFOUND :name1])
    =not=>          {:NOTFOUND {:name1 1}}))

;;; Sequences


(fact 
  (map-first str [1 2 3]) => ["1" 2 3])


(fact
  (vertical-slices [1 2 3]
                   [:a :b :c])
  => [ [1 :a] [2 :b] [3 :c]])

;;; Control flow

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

