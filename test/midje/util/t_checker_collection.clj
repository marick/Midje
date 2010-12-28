(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))

(fact "left-hand-side: sequentials that are to contain things"
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

  ;; containing maps
  [ {:a 1} "irrelevant"] => (contains {:a 1})

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

  ;; old bugs
  ( (contains [true]) [1 2]) => falsey
  ( (contains ['a]) [1 2]) => falsey
)

(fact "left-hand-side: actual return values that are strings"
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

  (chatty-falsehood-to-map ( (contains #"a" :in-any-order) "a"))
  => (contains {:actual "a", :notes (just #"regular expression.*:in-any-order")})
  ["a"] =>  (contains #"a" :in-any-order) ; this is OK because the singleton becomes a vector

  (chatty-falsehood-to-map ( (contains #"a" :gaps-ok) "a"))
  => (contains {:actual "a", :notes (just #"regular expression.*:gaps-ok")})
  ["a"] =>  (contains #"a" :gaps-ok) ; this is OK because the singleton becomes a vector

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

  ;; Strings and characters
  "s" => (just \s)
  ( (just \s) "as") => falsey
  "s" => (contains \s)
  ( (contains \s) "family") => falsey
  "as" => (contains [\s \a] :in-any-order)
  ( (contains [\s \a] :in-any-order) "af") => falsey
  "afs" => (contains [\s \a] :in-any-order :gaps-ok)
  ( (contains [\s \a] :in-any-order :gaps-ok) "af     S") => falsey

  ( (just "ab") [\a \b]) => falsey
  [\a \b] => (just (vec "ab"))

  ( (just "ab") "AB") => falsey
  "AB" => #"(?i)ab"
  
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

(fact "left-hand-side: sets to contain things"
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

  ;; containing maps
  #{ {:a 1} "irrelevant"} => (contains {:a 1})

  #{1} => (contains 1)
  #{1} => (just 1)

  (chatty-falsehood-to-map ( (has-prefix 1) #{1}))
  => (contains {:actual #{1} :notes ["Sets don't have prefixes."]})
  (chatty-falsehood-to-map ( (has-suffix 1) #{1}))
  => (contains {:actual #{1} :notes ["Sets don't have suffixes."]})
  (chatty-falsehood-to-map ( (has-prefix 1) {:a 1}))
  => (contains {:actual {:a 1} :notes ["Maps don't have prefixes."]})
  (chatty-falsehood-to-map ( (has-suffix 1) {:a 1}))
  => (contains {:actual {:a 1} :notes ["Maps don't have suffixes."]})
  )

(fact "left-hand-side: maps"
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

 (chatty-falsehood-to-map ( (contains [:a 1]) {:a 1}))
 => (contains {:actual {:a 1} :notes (just #"\{:a 1\} is a map.*\[:a 1\]")})
 ;; By the way, that means it'll be counted as false:
 ( (contains [:a 1]) {:a 1}) => chatty-checker-falsehood?

 ( (contains [1]) {:a 1}) => (contains {:actual {:a 1}
				      :notes (just #"\{:a 1\} is a map.*\[1\]")})
 ( (contains 1) {:a 1}) => (contains {:actual {:a 1}
				      :notes (just #"\{:a 1\} is a map.*1")})
 ((contains [:k :v]) {:k :v}) => (contains {:actual {:k :v}
					    :notes (just #"should look like map entries")})

 ;; Quantifiers
 {:a 1, :b 5, :c 3} => (has every? odd?)
 )

(facts "where actual values are of wrong type for legitimate expected"

  (chatty-falsehood-to-map ( (just "string")        1))
  => (contains {:actual 1})
  (chatty-falsehood-to-map ( (just {:a 1})        1))
  => (contains {:actual 1 :notes (just #"compare 1.*to \{:a 1\}")})
  (chatty-falsehood-to-map ( (contains \s)          1))
  => (contains {:actual 1 :notes (just #"compare 1.*to \\s")})
  ( (contains [1 2])       1)
  => (contains {:actual 1 :notes (just #"compare 1.*to \[1 2\]")})
  (println 'FIX)
  ( (contains #"ab")       1) => (exactly false)
  ( (just #{1})            [1 1]) => (exactly false)
  ( (contains {:a {:b 1}}) {:a 1}) => (exactly false)
  )

(fact "propagation of chatty failures"
  (println 'fix)
;  (chatty-falsehood-to-map ( (contains :a)        {:a 1}))
;  => (contains {:actual [1 2], :notes (just #"\{:a 1\}.*:a.*map entries")})
  )
  
(facts "about the notes given to reporting functions"
  "functions and such are printed nicely in the actual match section"
  (chatty-falsehood-to-map ( (contains [#"1" #"1+" #"1+2"]) [#"1" #"1+"]))
  => (contains {:notes (contains #"Best match.*\[#\"1\" #\"1\+\"\]")})

  ; It'd be nice to make all kinds of recursive function printing work nicely.
  ; [odd? even?] => (contains [(exactly odd?) (exactly odd?)])
  
  "checkers are printed nicely in the expected matched: section"
  (chatty-falsehood-to-map ( (contains [5 (exactly 4)] :in-any-order) [1 2 4]))
  => (contains {:notes (contains #"It matched.*\[\(exactly 4\)\]")})

  (chatty-falsehood-to-map ( (contains [(just 3) 6]) [[3] 5]))
  => (contains {:notes (contains #"It matched.*\[\(just 3\)\]")})

  (chatty-falsehood-to-map ( (contains [(contains 3) 6]) [[3] 5]))
  => (contains {:notes (contains #"It matched.*\[\(contains 3\)\]")})

  (chatty-falsehood-to-map ( (contains [(has-prefix 3) 6]) [[3] 5]))
  => (contains {:notes (contains #"It matched.*\[\(has-prefix 3\)\]")})

  (chatty-falsehood-to-map ( (contains [(has-suffix 3) 6]) [[3] 5]))
  => (contains {:notes (contains #"It matched.*\[\(has-suffix 3\)\]")})

  (chatty-falsehood-to-map ( (contains [#"fo+\[" "ba"]) ["foo[" "bar"]))
  => (contains {:notes (contains #"It matched.*\[#\"fo\+\\\[\"\]")})

  (chatty-falsehood-to-map ( (contains [1 "1\"2" [even?] odd?]) [1 "1\"2" [3]]))
  => (contains {:notes (contains #"It matched.*\[1 \"1\\\"2\"\]")})

  "Proper grammar for just errors"
  (chatty-falsehood-to-map ( (just 1) [1 2]))
  => (contains {:notes ["Expected one element. There were two."]})
  (chatty-falsehood-to-map ( (just 1) []))
  => (contains {:notes ["Expected one element. There were zero."]})
  (chatty-falsehood-to-map ( (just []) [1]))
  => (contains {:notes ["Expected zero elements. There was one."]})
  (chatty-falsehood-to-map ( (just [1 2]) [1]))
  => (contains {:notes ["Expected two elements. There was one."]})
)

(facts "where expected values are of wrong type for legitimate actual"
  (println 'fix)
  ( (just "hi")          '(1)) => (exactly false)
  ( (just (atom 0))      '(0)) => (exactly false)
  (chatty-falsehood-to-map ( (contains :a)        {:a 1}))
  => (contains {:actual {:a 1} :notes (just #"\{:a 1\}.*:a.*map entries")})
  (chatty-falsehood-to-map ( (contains 1)         {:a 1}))
  => (contains {:actual {:a 1} :notes (just #"\{:a 1\}.*1.*map entries")})

  (println 'fix)
  ( (contains (atom 0))  #{1}) => (exactly false)
  )

  
