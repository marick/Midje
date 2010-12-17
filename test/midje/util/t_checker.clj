(ns midje.util.t-checker
  (:use [midje.sweet])
  (:use [midje.test-util]))

(facts "about extended equality"
  (extended-= 1 2) => falsey
  (extended-= 1 odd?) => truthy

  (let [checker (fn [expected] (chatty-checker [actual] (> (inc actual) expected)))]
    (extended-= 5 ((checker 5) 4)) => falsey)

  "regexps"
  (extended-= #"a*b+" #"a*b+") => truthy
  (extended-= #"a*b+" #"a*b") => falsey
  (extended-= "BEGIN aab END" #"a*b+") => truthy
  (extended-= "BEGIN bb END" #"ab+") => falsey)

(facts "about truthy"
  true => truthy
  1 => truthy
  (truthy false) => false
  (truthy nil) => false)

(facts "about falsey"
  false => falsey
  nil => falsey
  (falsey true) => false
  (falsey 1) => false)

(facts "about anything"
  true => anything
  false => anything
  even? => anything)

(facts "about exactly"
  true => (exactly true)
  ( (exactly 2) 2) => truthy
  ( (exactly 1) 2) => falsey
  even? => (exactly even?))

(facts "about in-any-order"
  [] => (in-any-order [])
  [1] => (in-any-order [1])
  '(2 1) => (in-any-order [1 2])
  [ {:a 1} {:b 2} ] => (in-any-order [{:b 2} {:a 1}])

  ( (in-any-order [1 2]) [1 2 3]) => falsey
  ( (in-any-order [1 2]) [1]) => falsey
  ( (in-any-order [1 2]) [1 3]) => falsey
  
  ( (in-any-order [1 2 2 3]) [1 2 3 3]) => falsey
  ( (in-any-order [2 1 3 2]) [1 2 3 3]) => falsey)

(facts "about map-containing"
  {:a 1 :b 2} => (map-containing {:a 1 :b 2})
  {:a 1 :b 2 :c 3} => (map-containing {:a 1 :b 2})

  ( (map-containing {:a 1 :b 2})  {:a 1}) => falsey
  ( (map-containing {:a 1 :b 2})  {:a 1 :b 3}) => falsey)

(facts "about only-maps-containing-test"
  ( (only-maps-containing {:a 1 :b 2}) [{:a 1 :b 2} {:extra true}]) => falsey
  ( (only-maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey

  [{:a 1 :b 2} {:extra 1}] => (only-maps-containing {:extra 1} {:a 1})
  [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22}] => (only-maps-containing [{:b 2} {:b 22}])
  ( (only-maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey)

(facts "about maps-containing"
  ( (maps-containing {:a 1 :b 2}  {:extra true}) [{:a 1 :b 2}]) => falsey

  [{:a 1 :b 2} {:extra 1}] => (maps-containing {:extra 1} {:a 1})
  [{:a 1 :b 2} {:a 1 :b 22}] => (maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing {:b 2} {:b 22})
  [{:a 1 :b 2} {:a 1 :b 22} {:a 1 :b 33}] => (maps-containing [{:b 2} {:b 22}])
  ( (maps-containing {:b 2} {:b 22}) [{:b 2} {:b 33}]) => falsey)


(defn throw-exception
  ([] (throw (NullPointerException.)))
  ([message] (throw (Error. message)))
)

(facts "about throws"
  (throw-exception) => (throws NullPointerException)
  (throw-exception "hi") => (throws Error "hi")
  (throw-exception "hi") => (throws Error #"h."))

(after-silently 
 (fact 
   (throw-exception "throws Error") => (throws NullPointerException)
   (throw-exception "throws Error") => (throws Error "bye"))
 (fact 
   @reported => (two-of checker-fails)))

;; Unexpected exceptions
(after-silently
 (facts
   (throw-exception "throws Error") => anything
   (throw-exception "throws Error") => falsey
   (throw-exception "throws Error") => truthy)
 (fact
   @reported => (three-of checker-fails)))

(facts "about chatty-checking utility functions"
  (tag-as-chatty-falsehood [5]) => chatty-checker-falsehood?

  (chatty-worth-reporting-on? 1) => falsey 
  (chatty-worth-reporting-on? '()) => falsey
  (chatty-worth-reporting-on? '(f)) => truthy
  (chatty-worth-reporting-on? ''(f)) => truthy
  (chatty-worth-reporting-on? '[f]) => falsey

  (chatty-untease 'g-101 '()) => [[] []]
  
  (chatty-untease 'g-101 '(1 (f) 33 (+ 1 2))) =>
                [ '( (f) (+ 1 2))  '(1 (g-101 0) 33 (g-101 1))  ])
  

;; The form of chatty checkers

(def actual-plus-one-equals-4 (chatty-checker [actual] (= (inc actual) 4)))
(def no-longer-limited-form (chatty-checker [actual] (= (inc actual) 4 (+ 2 actual))))

(facts "about the form of chatty-checkers"
  actual-plus-one-equals-4 => chatty-checker?
  no-longer-limited-form => chatty-checker?)

(facts "about what chatty-checkers return"
  (actual-plus-one-equals-4 3) => true
   
  (let [result (actual-plus-one-equals-4 4)]
    result => chatty-checker-falsehood?
    result => {:actual 4
  	      :intermediate-results [ ['(inc actual) 5] ] })

  (let [result (no-longer-limited-form 4)]
    result => chatty-checker-falsehood?
    result => {:actual 4
  	      :intermediate-results [ ['(inc actual) 5] ['(+ 2 actual) 6] ]}))
    
(facts "about of-functions"
  [ 33 33 ] => (two-of 33)
  
  [ 1 3 ] => (n-of odd? 2)
  [ "ab" "aab" "aaab"] => (n-of #"a+b" 3)
  ( (n-of odd? 1) [1 3]) => chatty-checker-falsehood?
  ( (n-of odd? 3) [1 2 3]) => chatty-checker-falsehood?

  [1 1 3 3 5 5 7 7 9 9] => (ten-of odd?)
  [1 1 3 3 5 5 7 7 9] => (nine-of odd?)
  [1 1 3 3 5 5 7 7] => (eight-of odd?)
  [1 1 3 3 5 5 7] => (seven-of odd?)
  [1 1 3 3 5 5] => (six-of odd?)
  [1 1 3 3 5] => (five-of odd?)
  [1 1 3 3] => (four-of odd?)
  [1 1 3] => (three-of odd?)
  [1 1] => (two-of odd?)
  [1] => (one-of odd?))

(facts "about contains"
  "maps"
  {} => (contains {})
  {:k :v} => (contains {})
  {:k :v, 1 2} => (contains {:k :v})
  {:k :v, 1 2} => (contains {1 even?})
  ( (contains {:k :v}) {}) => falsey

  "works for sorted-maps in same way as for maps"
  (sorted-map "b" 1, "a" 2) => (contains {"b" 1})
  (sorted-map "b" 1, "a" 2) => (contains (sorted-map "a" 2))

  "maps can contain individual entries"
  {:k :v} => (contains [:k :v])
  {:k :v} => (contains (find {:k :v} :k))
  ((contains :k) {:k :v}) => (throws Error)

  "lists"
  '() => (contains '())
  '(1) => (contains '()) 
  '(1) => (contains '(1)) 
  '(1 2 3) => (contains '(1))
  '(1 2 3) => (contains '(2))
  '(1 2 3) => (contains '(3))
  '(1 2 3) => (contains '(2 3))
  '(3 2 1) => (contains '(1))
  '(1 3 1 2) => (contains '(1 2))
  '(1 3 2 3 1 2) => (contains '(1 2))
  ( (contains '(1 2)) '(1 3 2 3)) => falsey
  ( (contains '(1)) '()) => falsey
  ( (contains '(1 2)) '(1)) => falsey
  ( (contains '(1)) '(2)) => falsey

  '(1 nil 2 3 nil) => (contains (list odd? nil even? odd? nil?))
  ( (contains '(1 2)) '(3 2 1)) => falsey ; order matters
  ( (contains '(1 2 2 1)) '(1 2 1)) => falsey ; duplicates matter
  ( (contains '(1 2 1)) '(1 2 2 1)) => falsey ; duplicates matter

  "can contain single elements"
  '(1 2 3) => (contains 3)

  "vectors"
  [3 2 1] => (contains [1])
  [1 nil 2 3 nil] => (contains [odd? nil even? odd? nil?])
  ( (contains [1 2]) [3 2 1]) => falsey ; order matters
  ( (contains [2 2]) [2]) => falsey ; duplicates matter

  "seqs"
  (range 33) => (contains [16 17 18])
  (range 33) => (contains (range 16 3))

  "When sets are contained by sequentials, order is irrelevant"
  [3 2 1] => (contains #{2 1})
  ( (contains #{2 1 5}) [3 2 1]) => falsey

  "mixtures"
  [3 2 1] => (contains '(1))
  '(3 2 1) => (contains [1])

  [ {:a 1} {:b 1}      ] => (contains [ {:a 1} ])
  [ {:a 1} "irrelevant"] => (contains   {:a 1})

  ( (contains [ {:a 1} ])  [ {:a 1, :b 1} ]) => falsey  
  ( (contains {:a 1}) [ {:a 2} ]) => falsey
  ( (contains {:a 1}) [ 1 2 3 ]) => falsey
  ( (contains {:a 1}) [ [:a 1] ]) => falsey ; I suppose could arguably be true.

  "strings"
  "abc" => (contains "bc")
  "ab" => (contains "ab")
  ( (contains "ab") "ba") => falsey
  ( (contains "ab") "a") => falsey

  "strings can match collections, either singly or a a collection of strings"
  ["a" "bc" "c"] => (contains "bc")
  '("a" "bc" "c") => (contains "bc")
  ["1" "1 2" "1 2 3"]  => (contains ["1" "1 2"])
  #{"a" "bc" "c"} => (contains "bc")
  ( (contains "bc") {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  "regexp"
  "abc" => (contains #"bc")
  "ab" => (contains #"ab")
  "ab" => (contains #"..")
  "ab" => (contains #".")
  ( (contains #"ab") "ba") => falsey
  ( (contains #"ab") "a") => falsey

  "regexps can match expressions"
  ["a" "bc" "c"] => (contains #".c")
  #{"a" "bc" "c"} => (contains #"b5*c")
  '("a" "bc" "c") => (contains #"..")
  '("a" "bc" "c") => (contains [#"." #".."])
  ( (contains #"bc") {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  ;; Since #"regexp" is not = to #"regexp", no point in following:
  ;; [#"a" #"bc" #"c"] => (contains #"bc")

  ;; "sets"
  #{3 2 1} => (contains #{1})
  #{3 2 1} => (contains #{1 2})
  #{3 2 1} => (contains [1])   ; expected needn't be a set
  #{3 2 1} => (contains [1 3])   ; expected needn't be a set
  ( (contains [1 3]) #{1 2 4}) => falsey
  ( (contains #{1 3}) #{1 2 4}) => falsey
  ( (contains [1 1]) #{1 2 4}) => falsey
  #{3 2 1} => (contains odd?)
  #{3 2 1} => (contains #(= % 1))
  #{3 2 1} => (contains #{#(= % 1)})
  ( (contains #{#(= % 1) odd?}) #{2 1}) => falsey

  "nils"
  [nil] => (contains [nil])
  [nil nil nil] => (contains [nil nil])
  {:a nil, nil :a, :b 1} => (contains {:a nil, nil :a})
  #{nil 1} => (contains nil)
  #{nil 1} => (contains #{nil})
  #{nil 1} => (contains [1 nil])
  [nil "foo"] => (contains "foo")

  ( (contains [nil]) []) => falsey
  ( (contains [nil]) {}) => (throws Error)
  ( (contains [nil]) #{}) => falsey

  "individual elements"
  [1 2 3] => (contains 2)
  [1 2 3] => (contains even?)
  #{3 2 1} => (contains even?)
  [nil nil] => (contains nil)
  #{nil 1} => (contains nil)
  )
