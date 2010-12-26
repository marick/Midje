(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))


(fact "sequentials that are to contain things"
  [3 4 5 700] => (contains [4 5 700])
  ( (contains [4 5 700]) [4 700 5]) => falsey
  ( (contains [4 5 700]) [4 5 'hi 700]) => falsey

  ['hi 700 5 4] => (contains [4 5 700] :in-any-order)
  ( (contains [4 5 700] :in-any-order) [4 5 'hi 700]) => falsey

  [4 5 'hi 700] => (contains [4 5 700] :gaps-ok)
  ( (contains [4 5 700] :gaps-ok) [4 700 'hi' 5]) => falsey

  [4 700 5] => (contains [4 5 700] :gaps-ok :in-any-order)
  [4 5 'hi 700] => (contains [4 5 700] :in-any-order :gaps-ok)
  [700 'hi 4 5 'hi] => (contains [4 5 700] :in-any-order :gaps-ok)

  ;; containing sets
  [700 4 5] => (contains #{4 5 700})
  [700 4 5] => (contains #{4 5 700} :in-any-order) ; redundant
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok)
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok :in-any-order) ; redundant


  ;; Just
  [1 2 3] => (just [1 2 3]) 
  ( (just [1 2 3 4]) [1 2 3]) => falsey 
  ( (just [1 2 3]) [1 2 3 4]) => falsey 

  [1 2 3] => (just [odd? even? odd?])

  [1 3 2] => (just [1 2 3] :in-any-order)
  [1 3 2] => (just #{1 2 3})
  [1 3 2] => (just [1 2 3] :gaps-ok :in-any-order)  ;; silly
  [1 3 2] => (just #{1 2 3} :gaps-ok)


  [1 2 3] => (has-prefix [1 2])
  ( (has-prefix [2 1]) [1 2 3]) => false
  [1 2 3] => (has-prefix [2 1] :in-any-order)
  [1 2 3] => (has-prefix #{2 1})
  [1 2 3] => (has-prefix #{2 1} :gaps-ok)   ; silly

  [1 2 3] => (has-suffix [even? odd?])
  ( (has-suffix [odd? even?]) [1 2 3]) => falsey
  [1 2 3] => (has-suffix [odd? even?] :in-any-order)
  [1 2 3] => (has-suffix #{even? odd?})
  [1 2 3] => (has-suffix #{odd? even?} :gaps-ok)   ; silly

  ;; Singletons
  [700 4 5] => (contains 4)
  [4] => (just 4)
  [4] => (has-prefix 4)
  [4] => (has-suffix 4)

  [4 4 1] => (has some odd?)
  [1 3 5] => (has every? odd?)
  ( (has some odd?) [34 34 88]) => falsey
  ( (has every? odd?) [1 3 44]) => falsey
)

(fact "actual return values that are strings"
  "abc" => (contains "bc")
  ( (contains "bc") "bd") => falsey
  
  "abc" => (contains "ac" :gaps-ok) 
  "abc" => (contains "ba" :in-any-order)

  "abc" => (just "abc")
  ( (just "ab") "abc") => falsey
  
  ( (just "ac" :gaps-ok) "abc") => falsey
  "abc" => (just "cba" :in-any-order)
  ( (just "cba" :in-any-order) "ab") => falsey
  ( (just "cba" :in-any-order) "abcd") => falsey

  "abc" => (has-suffix "bc") 
  "abc" => (has-prefix "ab") 
  ( (has-suffix "ac") "abc") => falsey
  ( (has-suffix "ap") "abc") => falsey

  "123" => (has every? #(Character/isDigit %))
  ( (has every? #(Character/isDigit %)) "23a") => falsey

  ;; Comparisons to regular expressions
  "  1" => #"\d"

  "  3" => (contains #"\d")  
  ( (contains #"\d") "   ") => falsey

  "123" => (just #"\d\d\d")
  ( (just #"\d\d\d") "1234") => falsey
  ( (just #"\d\d\d") "12") => falsey

  "12x" => (has-prefix #"\d+")
  "x12" => (has-suffix #"\d+")

  ( (has-suffix #"\d+") "12x") => falsey
  ( (has-prefix #"\d+") "x12") => falsey

  ( (contains #"a" :in-any-order) "a") => (throws Error) 
  ( (contains #"a" :gaps-ok) "a") => (throws Error)

  ;; collections compared to strings or regular expressions
  ["a" "b" "c"] => (contains "b")
  ( (contains "b") ["a" "c"]) => falsey
  ["a" "b" "c"] => (contains ["b"])

  ["a" "b" "c"] => (contains #"b+")
  ( (contains #"b+") ["a" "c"]) => falsey
  ["a" "b" "c"] => (contains [#"b+"])
  ( (contains #"b+") ["a" "c"]) => falsey

  ["b"] => (just "b")
  ( (just "b") ["b" "c"]) => falsey
  ["b"] => (just ["b"])

  ["b"] => (just #"b+")
  ( (just #"b+") ["b" "c"]) => falsey
  ["b"] => (just [#"b+"])

  ["b" "c"] => (has-prefix "b")
  ( (has-prefix "b") ["c" "b" "c"]) => falsey
  ["b" "c"] => (has-prefix ["b"])

  ["b" "c"] => (has-prefix #"b+")
  ( (has-prefix #"b+") ["" "c"]) => falsey
  ["b" "c"] => (has-prefix [#"b+"])

  ["a" "b"] => (has-suffix "b")
  ( (has-suffix "b") ["b" "c"]) => falsey
  ["a" "b"] => (has-suffix ["b"])

  ["a" "b"] => (has-suffix #"b+")
  ( (has-suffix #"b+") ["b" "c"]) => falsey
  ["a" "b"] => (has-suffix [#"b+"])

  ;; Just to check
  ( (contains "a") [1]) => falsey
  ( (contains #"a") [1]) => falsey
  ( (just "a") [1]) => falsey
  ( (just #"a") [1]) => falsey
  ( (has-prefix "a") [1]) => falsey
  ( (has-prefix #"a") [1]) => falsey
  ( (has-suffix "a") [1]) => falsey
  ( (has-suffix #"a") [1]) => falsey
)

(fact "sets to contain things"
  #{1 2 3} => (contains #{1 2 3})
  #{1 2 3} => (contains [1 2 3])
			
  #{"1" "12" "123"} => (contains [#"1" #"2" #"3"])
  #{"1" "12" "123"} => (contains [#"1" #"2" #"3"])
  #{"1" "12" "123"} => (contains #{#"1" #"2"})
  ( (contains #{#"1" #"2" #"3"}) #{"1" "12"}) => falsey
  ( (contains #{#"1" #"3"}) #{"1" "12"}) => falsey

  #{"1" "12" "123"} => (contains [#"1" #"2" #"3"] :in-any-order) ; silly
  #{"1" "12" "123"} => (contains #{#"1" #"2" #"3"} :gaps-ok) ; silly

  #{"1" odd? 1} => (just [#"1" 1 (exactly odd?)])
  #{"1" "12" "123"} => (just [#"1" #"2" #"3"])
  ( (just #{#"1" #"2" #"3"}) #{"1" "12" "4"}) => falsey
  ( (just #{#"1" #"2" #"3"}) #{"1" "12" "123" "1234"}) => falsey

  #{"1" "12" "123"} => (just [#"1" #"2" #"3"] :in-any-order) ; silly
  #{"1" "12" "123"} => (just #{#"1" #"2" #"3"} :gaps-ok) ; silly
  )

(fact "maps"
 {:a 1, :b 2} => (contains {:a 1, :b 2})
 {:a "1", :b "2", :c "3"} => (contains {:a "1", :b "2"})
 ( (contains {:a 1, :b 2, :c 2}) {:a 1, :b 2}) => falsey
 ( (contains {:a 1, :c 2}) {:a 1, :b 2}) => falsey
 ( (contains {:a 1, :b 'not-2}) {:a 1, :b 2}) => falsey

 {:a 1, :b 2} => (contains {:a odd?, :b even?})
 {:a "1", :b "2"} => (contains {:a #"1", :b #"2"})
 {:a 1, :b 2} => (contains {:a odd?})
 (  (contains {:a even?}) {:a 1, :b 2}) => falsey

 {:a 1, :b 2} => (just {:a odd?, :b even?})
 ( (just {:a odd?}) {:a 1, :b 2}) => falsey
 (  (just {:a even?}) {:a 1}) => falsey
 (  (just {:a even?}) {nil 1}) => falsey

 ;; extended-equality isn't recursive, so...
 ;; ... while this works without lower-level annotation
 {:actual-found ["12" "1" "123"] } => (contains {:actual-found ["12" "1" "123"] })
 ;; ... this requires it:
 {:expected-found [#"2" #"1" #"3"] }
 => (contains {:expected-found (just [#"2" #"1" #"3"]) })
 
 {} => (contains {})
 {nil nil} => (contains {})
 {nil nil} => (contains {nil nil})
 ( (contains {nil nil}) {nil true}) => falsey
 ( (contains {nil nil}) {true nil}) => falsey

 ;; Map Entries
 {:a 1} => (contains [ (find {:a 1} :a) ])
 ((contains [ (find {:a 1} :a) ]) {:a 2}) => falsey

 {:a 1} => (just [ (find {:a 1} :a) ])
 {:a 1} => (just [ [:a 1] ])
 ( (just [ [:a 1] ]) {:a 1, :b 1}) => falsey

 ( (contains [:a 1]) {:a 1}) => (throws Error)
 ( (contains [1]) {:a 1}) => (throws Error)
 ( (contains 1) {:a 1}) => (throws Error)

 ;; Quantifiers
 {:a 1, :b 5, :c 3} => (has every? odd?)
 )

(future-fact "all the right-hand-side things"
	     ( (contains [1 2]) 1) => (throws Error)
)

  
