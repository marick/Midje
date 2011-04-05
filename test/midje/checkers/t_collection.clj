;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.t-collection
  (:use [midje sweet test-util]
        [midje.checkers.defining :only [checker?]]
        [midje.checkers.chatty :only [chatty-falsehood-to-map
                                      chatty-checker-falsehood?]]))
(testable-privates midje.checkers.collection separate-looseness)

(defrecord AB [a b])
(defrecord AB-different-class [a b])

(facts "demonstrating the effects of :in-any-order and ambiguity"
  ;; In case anyone wants to make the algorithm better.
  ;; Annoyingly, I seem to have mislaid some more extensive tests. Sloppy.
  ;; So be extra-careful changing the code - these tests are probably not enough.
  ["123" "12" "1"] => (contains [#"1" #"2" #"3"] :in-any-order)
  ["12" "123" "1"] => (contains [#"1" #"2" #"3"] :in-any-order)
  ["123" "1" "12" ] => (contains [#"1" #"2" #"3"] :in-any-order)
  ["1" "12" "123"] => (contains [#"1" #"2" #"3"] :in-any-order)

  [0 3 2 'a 1] => (contains #(<= % 3) #(<= % 2) #(<= % 1) :in-any-order :gaps-ok)
  [0 3 'a 1 2] => (contains #(<= % 3) #(<= % 2) #(<= % 1) :in-any-order :gaps-ok)
  [0 2 3 'a 1] => (contains #(<= % 3) #(<= % 2) #(<= % 1) :in-any-order :gaps-ok))

  

(fact "left-hand-side: sequentials that are to contain things"
  [3 4 5 700]        => (contains [4 5 700])
  [4 700 5]     =deny=> (contains [4 5 700])
  [4 5 'hi 700] =deny=> (contains [4 5 700])

  ['hi 700 5 4]      => (contains [4 5 700] :in-any-order)
  [4 5 'hi 700] =deny=> (contains [4 5 700] :in-any-order)

  [4 5 'hi 700]      => (contains [4 5 700] :gaps-ok)
  [4 700 'hi 5] =deny=> (contains [4 5 700] :gaps-ok)

  
  [4 700 5] => (contains [4 5 700] :gaps-ok :in-any-order)
  [4 5 'hi 700] => (contains [4 5 700] :in-any-order :gaps-ok)
  [700 'hi 4 5 'hi] => (contains [4 5 700] :in-any-order :gaps-ok)

  ;; containing sets
  [700 4 5] => (contains #{4 5 700})
  [700 4 5] => (contains #{4 5 700} :in-any-order) ; redundant
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok)
  [700 [] 4 5] => (contains #{4 5 700} :gaps-ok :in-any-order) ; redundant

  ;; containing maps and records
  [ {:a 1} "irrelevant"] => (contains {:a 1})
  [ (AB. 1 2) "irrelevant"] => (contains (AB. 1 2))
  [ (AB. 1 2) "irrelevant"] => (contains [(AB. 1 2)])
  ( (contains (AB. 1 2)) [ 1 2 ] ) => falsey
  ( (contains [(AB. 1 2)]) [ [:a 1] [:b 2 ] ] ) => falsey

  ;; Just
  [1 2 3] => (just [1 2 3]) 
  ( (just [1 2 3 4]) [1 2 3]) => falsey 
  ( (just [1 2 3]) [1 2 3 4]) => falsey 

  [1 2 3] => (just [odd? even? odd?])

  [1 3 2] => (just [1 2 3] :in-any-order)
  [1 3 2] => (just #{1 2 3})
  [1 3 2] => (just [1 2 3] :gaps-ok :in-any-order)  ;; silly
  [1 3 2] => (just #{1 2 3} :gaps-ok)


  [1 2 3]     => (has-prefix [1 2])
  [1 2 3] =not=> (has-prefix [2 1])
  [1 2 3] => (has-prefix [2 1] :in-any-order)
  [1 2 3] => (has-prefix #{2 1})
  [1 2 3] => (has-prefix #{2 1} :gaps-ok)   ; silly

  [1 2 3]      => (has-suffix [even? odd?])
  [1 2 3] =deny=> (has-suffix [odd? even?])
  [1 2 3] => (has-suffix [odd? even?] :in-any-order)
  [1 2 3] => (has-suffix #{even? odd?})
  [1 2 3] => (has-suffix #{odd? even?} :gaps-ok)   ; silly

  ;; Singletons
  [700 4 5] => (contains 4 :in-any-order)
  [4] => (just 4)
  [4] => (has-prefix 4)
  [4] => (has-suffix 4)
  [:in-any-order] => (just :in-any-order :in-any-order)

  [4 4 1] => (has some odd?)
  [1 3 5] => (has every? odd?)
  ( (has some odd?) [34 34 88]) => falsey
  ( (has every? odd?) [1 3 44]) => falsey

  ;; More than one not enclosed in a collection
  [700 4 5] => (just 4 5 700 :in-any-order)
  [[700] [4] [5]] => (contains [700] [5] :gaps-ok)
  [ [1] [2] ] => (just (contains odd?) (contains even?))
  [ {:s 2} [2] ] => (just map? vector?)
  [ #{1 2} #{:a} {:a 1} #{3 4} ] => (contains #{1 2} #{3 4} :gaps-ok)

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
  "as" => (has-prefix \a \s)
  ( (has-prefix "a" "s") "as") => falsey
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
  #{1 2} => (contains 1 2)
  #{{:a 1} {:b 2}} => (contains [{:a 1} {:b 2}])
  #{{:a 1} {:b 2}} => (contains {:a 1} {:b 2})

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
 ( (contains (AB. 1 2)) {:a 1 :b 2}) => falsey
 ( (just {:top (just (AB. 1 2))}) {:top {:a 1, :b 2}}) => falsey

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
 {:a 1, :b 2, :c 3} => (contains   [:a 1] [:b 2]  ) ; Brackets can be dropped.
 ( (just [:a 1]) {:a 1}) => falsey ; ambiguity resolved in favor of array.

 {:a 1, :b 3} => (contains [:a 1] [:b odd?])
 ( (contains [:a 1] [:b odd?]) {:a 1, :b odd?}) => falsey
 {:a 1, :b odd?} => (contains [:a 1] [:b (exactly odd?)])
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

(facts "when the left-hand-side is a record"
  "... and so is the right-hand-side"
  (assoc (AB. 1 2) :c 3) => (contains (AB. 1 2))
  ( (just (AB. 1 2))  (assoc (AB. 1 2) :c 3)) => falsey
  (AB. 1 2) => (contains (AB. odd? even?))
  ( (just (AB. 1 2)) (AB-different-class. 1 2)) => falsey

  ;; collections of records, just for fun.
  #{ (AB. 1 2) (AB. 'a 'b) } => (contains (AB. 1 2) )
  {:a (AB. 1 2) } => (just {:a (AB. 1 2)})

  "... and the right-hand-side is a map"
  (AB. 1 2) => (contains {:a 1})
  (AB. 1 2) => (contains {:a 1, :b 2})
  (AB. 1 2) => (contains {:a 1, :b even?})
  ((contains {:a 1, :b 2, :c 3}) (AB. 1 2)) => falsey

  ;; collections of records, just for fun.
  [ (AB. 1 2) ] => (contains (AB. 1 2))
  [ (AB. 1 2) ] => (contains [(AB. 1 2)])

  ((just {:a 1}) (AB. 1 2)) => falsey
  (AB. 1 2) => (just {:a 1, :b 2})
  (AB. 1 2) => (just {:a 1, :b even?})
  ((just {:a 1, :b 2, :c 3}) (AB. 1 2)) => falsey
  {:a (AB. 1 2)} => (just {:a (AB. 1 2)})
  {:a (AB. 1 2)} => (contains {:a (AB. 1 2)})

  "... and the right hand side consists of map entries."
  (AB. 1 2) => (just [[:a 1] [:b 2]])
  (AB. 1 2) => (just [:a 1] [:b 2])
  (AB. 1 2) => (contains [[:a 1]]))
 
  

(facts "where actual values are of wrong type for legitimate expected"

  (chatty-falsehood-to-map ( (just "string")        1))
  => (contains {:actual 1})
  (chatty-falsehood-to-map ( (just {:a 1})        1))
  => (contains {:actual 1 :notes (just #"compare 1.*to \{:a 1\}")})
  (chatty-falsehood-to-map ( (contains \s)          1))
  => (contains {:actual 1 :notes (just #"compare 1.*to \\s")})
  ( (contains [1 2])       1)
  => (contains {:actual 1 :notes (just #"compare 1.*to \[1 2\]")})

  (chatty-falsehood-to-map ( (just #"ab")       1))
  => (contains {:actual 1 :notes (just #"#\"ab\" can't be used on 1")})
  (chatty-falsehood-to-map ( (contains #"ab")       1))
  => (contains {:actual 1 :notes (just #"#\"ab\" can't be used on 1")})
  (chatty-falsehood-to-map ( (has-prefix #"ab")       1))
  => (contains {:actual 1 :notes (just #"#\"\^ab\" can't be used on 1")})
  (chatty-falsehood-to-map ( (has-suffix #"ab")       1))
  => (contains {:actual 1 :notes (just #"#\"ab\$\" can't be used on 1")})

  (chatty-falsehood-to-map ( (contains {:a 1, :b 2}) {:a 1}))
  => (contains {:actual {:a 1} :notes (just "Best match found: {:a 1}.")})
  (chatty-falsehood-to-map ( (just {:a 1, :b 2}) {:a 1}))
  => (contains {:actual {:a 1} :notes (just #"Expected two elements.*one")})
  (chatty-falsehood-to-map ( (contains {:a {:b 1}}) {:a 1}) )
  => (contains {:actual {:a 1} :notes (just "Best match found: {}.")})
  (chatty-falsehood-to-map ( (contains {:a odd?, :f odd? :g odd?}) {:f 3, :g 6, :a 1}) )
  => (contains {:actual {:f 3, :g 6, :a 1}
                :notes (just [#"Best match found: \{:a 1, :f 3\}\."
                              #"It matched: \{:a odd\?, :f odd\?\}\."])})
  (chatty-falsehood-to-map ( (contains :a)        {:a 1}))
  => (contains {:actual {:a 1}, :notes (just #"\{:a 1\}.*:a.*map entries")})
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
  (chatty-falsehood-to-map ( (just #{1}) [1 1]))
  => (contains {:notes ["Expected one element. There were two."]})


  (chatty-falsehood-to-map ((has-prefix '(a b c)) '(a)))
  => (contains {:notes ["A collection with one element cannot match a prefix of size three."]})

  (chatty-falsehood-to-map ((has-suffix '(1)) '()))
  => (contains {:notes ["A collection with zero elements cannot match a suffix of size one."]})
)

(facts "where expected values are of wrong type for legitimate actual"
  (chatty-falsehood-to-map ( (just "hi")          '(1)))
  => (contains {:actual (list 1) :notes (just #"\[\]")})
  (chatty-falsehood-to-map ( (just (atom 0))      '(0)))
  => (contains {:actual '(0) :notes (just #"\[\]")})
  (chatty-falsehood-to-map ( (contains :a)        {:a 1}))
  => (contains {:actual {:a 1} :notes (just #"\{:a 1\}.*:a.*map entries")})
  (chatty-falsehood-to-map ( (contains 1)         {:a 1}))
  => (contains {:actual {:a 1} :notes (just #"\{:a 1\}.*1.*map entries")})
  (chatty-falsehood-to-map ( (just (AB. 1 2)) {:a 1 :b 2}))
  => (contains {:actual {:a 1 :b 2}
                :notes (just #"AB.*but.*was.*map")})
  (chatty-falsehood-to-map ( (just (AB. 1 2)) (AB-different-class. 1 2)))
  => (contains {:actual (AB-different-class. 1 2)
                :notes (just #"AB.*but.*was.*AB-different-class")})
  )

(fact "Actual result shown is the original collection"
             ;; This prints like this: {:actual [1], :notes (Best match found: [])}
  (str (:actual ( (contains (atom 0))  #{1}))) => "#{1}"
  (str (:actual ( (just (atom 0)) #{1}))) => "#{1}"
  (str (:actual ( (has-suffix [\a \b \c]) "many"))) => "many"
  (str (:actual ( (has-prefix 5) [#{{1 2}} 2 3 4]))) => "[#{{1 2}} 2 3 4]"
  )

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

(facts "about separating looseness from arguments"
  (separate-looseness [1]) => [ 1 [] ]
  (separate-looseness [1 :in-any-order]) => [ 1 [:in-any-order] ]
  (separate-looseness [1 :in-any-order :gaps-ok]) => [ 1 [:in-any-order :gaps-ok] ]
  (separate-looseness [1 2]) => [ [1 2] [] ]
  (separate-looseness [1 2 :in-any-order]) => [ [1 2] [:in-any-order] ]

  (separate-looseness [ [4 5 700] :in-any-order ]) => [ [4 5 700] [:in-any-order] ])

(facts "collection checkers are checkers"
  has => checker?
  has-suffix => checker?
  has-prefix => checker?
  just => checker?
  contains => checker?
  one-of => checker?

  #'has => checker?
  #'has-suffix => checker?
  #'has-prefix => checker?
  #'just => checker?
  #'contains => checker?
  #'two-of => checker?

  (has some even?) => checker?
  (has-suffix "foo") => checker?
  (has-prefix '(b)) => checker?
  (just 1) => checker?
  (contains [1 2] :in-any-order) => checker?
  (one-of even?) => checker?
  (n-of even? 33) => checker?)
