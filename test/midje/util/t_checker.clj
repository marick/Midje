(ns midje.util.t-checker
  (:use [midje.sweet])
  (:use [midje.test-util]))
(testable-privates midje.util.checkers unordered-seq-comparison actual-index-of
		   without-element-at-index)


(facts "about extended equality"
  (extended-= 1 2) => falsey
  (extended-= 1 odd?) => truthy

  (let [checker (fn [expected] (chatty-checker [actual] (> (inc actual) expected)))]
    (extended-= 5 ((checker 5) 4)) => falsey)

  "regexps"
  (extended-= #"a*b+" #"a*b+") => truthy
  (extended-= #"a*b+" #"a*b") => falsey
  (extended-= "BEGIN aab END" #"a*b+") => truthy
  (extended-= "BEGIN bb END" #"ab+") => falsey

  ;; When searching for unordered comparisons, you might get exceptions.
  ;; Count those as false.
  (extended-= nil odd?) => falsey)

(facts "actual-index-of"
  (actual-index-of 5 []) => false
  (actual-index-of 5 [5]) => 0
  (actual-index-of 5 [1 5]) => 1
  (actual-index-of 5 [2 3]) => false
  (actual-index-of 5 [1 2 5 5]) => 2 ; not 3
  (actual-index-of odd? [1 3 3]) => 0)

(facts "extract elements from vectors and return remainder"
  (without-element-at-index 0 [0 1 2]) => vector?
  (without-element-at-index 0 [0 1 2]) => [1 2]
  (without-element-at-index 1 [0 1 2]) => [0 2]
  (without-element-at-index 2 [0 1 2]) => [0 1])


(facts "unordered comparisons"
  (unordered-seq-comparison [] []) => {:actual-found []
				       :actual-missed []
				       :expected-found []
				       :expected-missed [] }
  (unordered-seq-comparison [1] []) => {:actual-found []
					:actual-missed [1]
					:expected-found []
					:expected-missed [] }
  (unordered-seq-comparison [] [1]) => {:actual-found []
					:actual-missed []
					:expected-found []
					:expected-missed [1] }
  (unordered-seq-comparison [1] [1]) => {:actual-found [1]
					 :actual-missed []
					 :expected-found [1]
					 :expected-missed [] }
  (unordered-seq-comparison [1 2] [1]) => {:actual-found [1]
					   :actual-missed [2]
					   :expected-found [1]
					   :expected-missed [] }
  (unordered-seq-comparison [] [1]) => {:actual-found []
					:actual-missed []
					:expected-found []
					:expected-missed [1] }
  (unordered-seq-comparison [1 2 3] [odd?]) => {:actual-found [1]
						:actual-missed [2 3]
						:expected-found [odd?]
						:expected-missed [] }
  (unordered-seq-comparison [1 2 3] [odd? even?]) => {:actual-found [1 2]
						      :actual-missed [3]
						      :expected-found [odd? even?]
						      :expected-missed [] }
  (unordered-seq-comparison [1 2 3] [even? odd?]) => {:actual-found [2 1]
  						      :actual-missed [3]
  						      :expected-found [even? odd?]
  						      :expected-missed [] }
  (unordered-seq-comparison [1 2 3] [odd? even? even?]) => {:actual-found [1 2]
							    :actual-missed [3]
							    :expected-found [odd? even?]
							    :expected-missed [even?] }
  (unordered-seq-comparison [nil] []) => {:actual-found []
					  :actual-missed [nil]
					  :expected-found []
					  :expected-missed [] }
  (unordered-seq-comparison [] [nil]) => {:actual-found []
					  :actual-missed []
					  :expected-found []
					  :expected-missed [nil] }
  (unordered-seq-comparison [1 2 3 nil] [odd? nil even?]) => {:actual-found [1 nil 2]
							      :actual-missed [3]
							      :expected-found [odd? nil even?]
							      :expected-missed [] })

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

(facts "about has-prefix"
  "lists"
  '() => (has-prefix '())
  '(1) => (has-prefix '()) 
  '(1) => (has-prefix '(1)) 
  '(1 2 3) => (has-prefix '(1))
  ((has-prefix '(2)) '(1 2 3)) => falsey
  '(1 2 3) => (has-prefix '(1 2))
  ((has-prefix '(1 3)) '(1 2 3)) => falsey
  ( (has-prefix '(1 2)) '(1 3 2 2)) => falsey
  ( (has-prefix '(1 2)) '(1 3 2 3 1 2)) => falsey
  ( (has-prefix '(1)) '()) => falsey
  ( (has-prefix '(1 2)) '(1)) => falsey
  ( (has-prefix '(1)) '(2)) => falsey

  '(1 nil 2 3 nil) => (has-prefix (list odd? nil even? odd? nil?))
  ( (has-prefix '(1 2)) '(2 1)) => falsey ; order matters
  ( (has-prefix '(1 2 1)) '(1 2 2 1)) => falsey ; duplicates matter

;;   "can contain single elements"
  '("1" 2 3) => (has-prefix "1")

;;   "vectors"
  [1 2] => (has-prefix [1])
  [1 nil 2 3 nil 5] => (has-prefix [odd? nil even? odd? nil?])
  ( (has-prefix [1 2]) [1 3]) => falsey
  ( (has-prefix [2 2]) [2]) => falsey ; duplicates matter

;;   "seqs"
  (range 33) => (has-prefix [0 1])
  (range 33) => (has-prefix (range 3))

;;   "When sets prefix sequentials, order is irrelevant"
   [1 2 3 5] => (has-prefix #{2 1 3})
   ( (has-prefix #{2 1 5}) [1 2 3]) => falsey

   ;; "maps"
   ((has-prefix :a) { :a 1 }) => falsey
   ((has-prefix :a) [{ :a 1 }]) => falsey
   ((has-prefix [:a]) {:a 1 }) => falsey

   ;; "sets"
   ((has-prefix :a) #{:a}) => falsey
   ((has-prefix #{:a}) #{:a 1}) => falsey
   ((has-prefix [:a]) #{:a 1}) => falsey

;;   "mixtures"
   [1 2 4] => (has-prefix '(1))
   '(3 b 1) => (has-prefix [3 'b])

   [ {:a 1} {:b 1}      ] => (has-prefix [ {:a 1} ])
   [ {:a 1} "irrelevant"] => (has-prefix   {:a 1})

   ( (has-prefix [ {:a 1} ])  [ {:a 1, :b 1} ]) => falsey  
   ( (has-prefix {:a 1}) [ {:a 2} ]) => falsey
   ( (has-prefix {:a 1}) [ 1 2 3 ]) => falsey
   ( (has-prefix {:a 1}) [ [:a 1] ]) => falsey ; I suppose could arguably be true.

;;   "strings"
   "ab" => (has-prefix "ab")
   "ab" => (has-prefix "a")
   ( (has-prefix "bc") "abc") => falsey
   ( (has-prefix "ab") "b") => falsey
   ( (has-prefix "ab") "a") => falsey

;;   "strings can match collections, either singly or a a collection of strings"
   ["a" "bc" "c"] => (has-prefix "a")
   ["1" "1 2" "1 2 3"]  => (has-prefix ["1" "1 2"])

   ;;   "regexp"
   "abc" => (has-prefix #"ab")
   "ab" => (has-prefix #"ab")
   "ab" => (has-prefix #"..")
   "ab" => (has-prefix #".")
   ( (has-prefix #"b.") "ab") => falsey
   ( (has-prefix #"ab") "ba") => falsey
   ( (has-prefix #"ab") "a") => falsey

   ;;   "regexps can match expressions"
   ["ac" "bd" "c"] => (has-prefix #".c")
   ( (has-prefix #".c") ["bd" "ac" "c"]) => falsey
   '("a" "bc" "cccc") => (has-prefix #".")
   '("a" "bc" "c") => (has-prefix [#"." #".."])
   ( (has-prefix [#"." #".."]) '("ab" "b" "cd")) => falsey

   "abc" => (has-prefix #"^ab")
  ( (has-prefix #"^ab") "xabc") => falsey
  "abc" => (has-prefix #"^abc$")
  ( (has-prefix #"^ab$") "abc") => falsey

   [#"bc" #"c"] => (has-prefix #"bc")

   ;; "nils"
   [nil] => (has-prefix [nil])
   [nil nil nil] => (has-prefix [nil nil])
   [nil "foo"] => (has-prefix nil)

   ( (has-prefix [nil]) []) => falsey

   ;;   "individual elements"
   [1 2 3] => (has-prefix 1)
   [1 2 3] => (has-prefix odd?)
   [nil nil] => (has-prefix nil)
  )


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
  "abc" => (contains #"^ab")
  ( (contains #"^ab") "xabc") => falsey
  "abc" => (contains #"^abc$")
  ( (contains #"^ab$") "abc") => falsey

  "regexps can match expressions"
  ["a" "bc" "c"] => (contains #".c")
  #{"a" "bc" "c"} => (contains #"b5*c")
  '("a" "bc" "c") => (contains #"..")
  '("a" "bc" "c") => (contains [#"." #".."])
  ( (contains #"bc") {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  [#"a" #"bc" #"c"] => (contains #"bc")

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


;.;. When someone asks you if you're a god, you say 'YES'! -- Zeddemore
(facts "about contains - in any order"
  "maps"
  {} => (contains {} :in-any-order)
  {:k :v} => (contains {} :in-any-order)
  {:k :v, 1 2} => (contains {:k :v} :in-any-order)
  {:k :v, 1 2} => (contains {1 even?} :in-any-order)
  ( (contains {:k :v} :in-any-order) {}) => falsey

  "works for sorted-maps in same way as for maps"
  (sorted-map "b" 1, "a" 2) => (contains {"b" 1} :in-any-order)
  (sorted-map "b" 1, "a" 2) => (contains (sorted-map "a" 2) :in-any-order)

  ;; "maps can contain individual entries"
  {:k :v} => (contains [:k :v] :in-any-order)
  {:k :v} => (contains (find {:k :v} :k) :in-any-order)
  ((contains :k :in-any-order) {:k :v}) => (throws Error)

  ;; "lists"
  '() => (contains '() :in-any-order)
  '(1) => (contains '() :in-any-order) 
  '(1) => (contains '(1) :in-any-order) 
  '(1 2 3) => (contains '(1) :in-any-order)
  '(1 2 3) => (contains '(2) :in-any-order)
  '(1 2 2 3) => (contains '(2 3 2) :in-any-order)
  ( (contains '(3 2 2) :in-any-order) '(1 2 3) ) => falsey
  '(1 2 3) => (contains '(3 2) :in-any-order) 

  '(1 2 3) => (contains '(2 3) :in-any-order)
  '(3 2 1) => (contains '(1) :in-any-order)
  '(1 3 1 2) => (contains '(1 2) :in-any-order)
  '(1 3 2 3 1 2) => (contains '(1 2) :in-any-order)
  '(1 3 2 3) =>  (contains '(1 2) :in-any-order)
  ( (contains '(1) :in-any-order) '()) => falsey
  ( (contains '(1 2) :in-any-order) '(1)) => falsey
  ( (contains '(1) :in-any-order) '(2)) => falsey

  '(1 nil 2 3 nil) => (contains (list odd? nil even? odd? nil?) :in-any-order)
  '(1 nil 2 3 nil) => (contains (list even? odd? odd? nil nil?) :in-any-order)
  '(3 2 1) => (contains '(1 2) :in-any-order)
  ( (contains '(1 2 2 1) :in-any-order) '(1 2 1)) => falsey ; duplicates matter
  '(1 2 2 1) => (contains '(1 2 1) :in-any-order)

  ;; "can contain single elements"
  '(1 2 3) => (contains 3 :in-any-order)

  ;; "vectors"
  [3 2 1] => (contains [1] :in-any-order)
  [1 nil 2 3 nil] => (contains [odd? even? odd? nil? nil] :in-any-order)
  [3 2 1] => (contains [1 2] :in-any-order) 
  ( (contains [2 2] :in-any-order) [2]) => falsey ; duplicates matter

  ;; "seqs"
  (range 33) => (contains (reverse [16 17 18]) :in-any-order)
  (range 33) => (contains (reverse (range 16 3)) :in-any-order)
  (reverse (range 33)) => (contains (range 16 3) :in-any-order)

  [3 2 1] => (contains #{2 1} :in-any-order)
  ( (contains #{2 1 5} :in-any-order) [3 2 1]) => falsey

  ;; "mixtures"
  [3 2 1] => (contains '(1) :in-any-order)
  '(3 2 1) => (contains [1 2] :in-any-order)

  [ {:a 1} {:b 1}      ] => (contains [ {:a 1} ] :in-any-order)
  [ {:a 1} {:b 1}      ] => (contains [ {:b 1} {:a 1} ] :in-any-order)
  [ {:a 1} "irrelevant"] => (contains   "irrelevant" :in-any-order)

  ( (contains [ {:a 1} ] :in-any-order)  [ {:a 1, :b 1} ]) => falsey  
  ( (contains {:a 1} :in-any-order) [ {:a 2} ]) => falsey
  ( (contains {:a 1} :in-any-order) [ 1 2 3 ]) => falsey
  ;; ( (contains {:a 1} :in-any-order) [ [:a 1] ]) => falsey ; I suppose could arguably be true.

  "strings"
  "abc" => (contains "bc" :in-any-order)
  ;; "abc" => (contains "ac" :in-any-order)
  "ab" => (contains "ab" :in-any-order)
  ;; "ab" => (contains "ba" :in-any-order)
  ( (contains "ab" :in-any-order) "a") => falsey

  "strings can match collections, either singly or a a collection of strings"
  ["a" "bc" "c"] => (contains "bc" :in-any-order)
  ;; ["a" "bc" "c"]) => (contains "cb" :in-any-order)
  '("a" "bc" "c") => (contains "bc" :in-any-order)
  ;; ["1" "1 2" "1 2 3"]  => (contains ["1 2" "1" ] :in-any-order)
  ( (contains ["1" "2 1"] :in-any-order) ["1" "1 2" "1 2 3"])  => falsey
  #{"a" "bc" "c"} => (contains "bc" :in-any-order)
  ( (contains "bc" :in-any-order) {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  ;; "regexp"
  ;; ( (contains #"bc" :in-any-order) "abc") => (throws Error)

  ;; "regexps can match expressions"
  ["a" "bc" "c"] => (contains #".c" :in-any-order)
  #{"a" "bc" "c"} => (contains #"b5*c" :in-any-order)
  '("a" "bc" "c") => (contains #".." :in-any-order)
  '("a" "bc" "c") => (contains [#".." #"."] :in-any-order)
  ( (contains #"bc" :in-any-order) {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  [#"a" #"bc" #"c"] => (contains #"bc" :in-any-order)
  ;; [#"a" #"bc" #"c"] => (contains [#"bc" #"a"] :in-any-order)

  ;; ;; "sets"
  #{3 2 1} => (contains #{1} :in-any-order)
  #{3 2 1} => (contains #{1 2} :in-any-order)
  #{3 2 1} => (contains [1] :in-any-order)   ; expected needn't be a set
  #{3 2 1} => (contains [1 3] :in-any-order)   ; expected needn't be a set
  ( (contains [1 3] :in-any-order) #{1 2 4}) => falsey
  ( (contains #{1 3} :in-any-order) #{1 2 4}) => falsey
  ;; ( (contains [1 1]) #{1 2 4} :in-any-order) => falsey
  #{3 2 1} => (contains odd? :in-any-order)
  #{3 2 1} => (contains #(= % 1) :in-any-order)
  #{3 2 1} => (contains #{#(= % 1)} :in-any-order)
  ( (contains #{#(= % 1) odd?} :in-any-order) #{2 1}) => falsey

  ;; "nils"
  [nil] => (contains [nil] :in-any-order)
  [nil nil nil] => (contains [nil nil] :in-any-order)
  {:a nil, nil :a, :b 1} => (contains {:a nil, nil :a} :in-any-order)
  #{nil 1} => (contains nil :in-any-order)
  #{nil 1} => (contains #{nil} :in-any-order)
  #{nil 1} => (contains [1 nil] :in-any-order)
  [nil "foo"] => (contains "foo" :in-any-order)

  ( (contains [nil] :in-any-order) []) => falsey
  ( (contains [nil] :in-any-order) {}) => (throws Error)
  ( (contains [nil] :in-any-order) #{}) => falsey

  ;; "individual elements"
  [1 2 3] => (contains 2 :in-any-order)
  [1 2 3] => (contains even? :in-any-order)
  #{3 2 1} => (contains even? :in-any-order)
  [nil nil] => (contains nil :in-any-order)
  #{nil 1} => (contains nil :in-any-order)
  )
