(ns midje.util.t-checker-extra-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))
(testable-privates midje.util.checkers unordered-seq-comparison index-of-actual)

;; These are still potentially useful test from a misguided organization.

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

  "can contain single elements"
  '("1" 2 3) => (has-prefix "1")

  "vectors"
  [1 2] => (has-prefix [1])
  [1 nil 2 3 nil 5] => (has-prefix [odd? nil even? odd? nil?])
  ( (has-prefix [1 2]) [1 3]) => falsey
  ( (has-prefix [2 2]) [2]) => falsey ; duplicates matter

  "seqs"
  (range 33) => (has-prefix [0 1])
  (range 33) => (has-prefix (range 3))

  "When sets prefix sequentials, order is irrelevant"
  [1 2 3 5] => (has-prefix #{2 1 3})
  ( (has-prefix #{2 1 5}) [1 2 3]) => falsey

  "maps"
  ((has-prefix :a) { :a 1 }) => falsey
  ((has-prefix :a) [{ :a 1 }]) => falsey
  ((has-prefix [:a]) {:a 1 }) => falsey

  "sets"
  ((has-prefix :a) #{:a}) => falsey
  ((has-prefix #{:a}) #{:a 1}) => falsey
  ((has-prefix [:a]) #{:a 1}) => falsey

  "mixtures"
  [1 2 4] => (has-prefix '(1))
  '(3 b 1) => (has-prefix [3 'b])

  [ {:a 1} {:b 1}      ] => (has-prefix [ {:a 1} ])
  [ {:a 1} "irrelevant"] => (has-prefix   {:a 1})

  ( (has-prefix [ {:a 1} ])  [ {:a 1, :b 1} ]) => falsey  
  ( (has-prefix {:a 1}) [ {:a 2} ]) => falsey
  ( (has-prefix {:a 1}) [ 1 2 3 ]) => falsey
  ( (has-prefix {:a 1}) [ [:a 1] ]) => falsey ; I suppose could arguably be true.

  "strings"
  "ab" => (has-prefix "ab")
  "ab" => (has-prefix "a")
  ( (has-prefix "bc") "abc") => falsey
  ( (has-prefix "ab") "b") => falsey
  ( (has-prefix "ab") "a") => falsey

  "strings can match collections, either singly or a a collection of strings"
  ["a" "bc" "c"] => (has-prefix "a")
  ["1" "1 2" "1 2 3"]  => (has-prefix ["1" "1 2"])

  "regexp"
  "abc" => (has-prefix #"ab")
  "ab" => (has-prefix #"ab")
  "ab" => (has-prefix #"..")
  "ab" => (has-prefix #".")
  ( (has-prefix #"b.") "ab") => falsey
  ( (has-prefix #"ab") "ba") => falsey
  ( (has-prefix #"ab") "a") => falsey

  "regexps can match collections"
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

  "nils"
  [nil] => (has-prefix [nil])
  [nil nil nil] => (has-prefix [nil nil])
  [nil "foo"] => (has-prefix nil)

  ( (has-prefix [nil]) []) => falsey
  
  "individual elements"
  [1 2 3] => (has-prefix 1)
  [1 2 3] => (has-prefix odd?)
  [nil nil] => (has-prefix nil)
  )

(facts "about has-prefix where elements can be in any order"
  "lists"
  '() => (has-prefix '() :in-any-order)
  '(1) => (has-prefix '() :in-any-order) 
  '(1) => (has-prefix '(1) :in-any-order) 
  '(1 2 3) => (has-prefix '(1) :in-any-order)
  ((has-prefix '(2) :in-any-order) '(1 2 3)) => falsey
  '(1 2 3) => (has-prefix '(1 2) :in-any-order)
  '(1 2 3) => (has-prefix '(2 1) :in-any-order)
  ((has-prefix '(1 3) :in-any-order) '(1 2 3)) => falsey
  ( (has-prefix '(1 2) :in-any-order) '(1 3 2 2)) => falsey
  ( (has-prefix '(1 2) :in-any-order) '(1 3 2 3 1 2)) => falsey
  ( (has-prefix '(1) :in-any-order) '()) => falsey
  ( (has-prefix '(1 2) :in-any-order) '(1)) => falsey
  ( (has-prefix '(1) :in-any-order) '(2)) => falsey

  '(1 nil 2 3 nil) => (has-prefix (list odd? nil even?) :in-any-order)
  '(1 nil 2 3 nil) => (has-prefix (list nil odd? even?) :in-any-order)
  ( (has-prefix '(1 2 1) :in-any-order) '(1 2 2 1)) => falsey ; duplicates matter

  "can contain single elements"
  '("1" 2 3) => (has-prefix "1" :in-any-order)

  "vectors"
  [1 2] => (has-prefix [1] :in-any-order)
  [1 nil 2 3 nil 5] => (has-prefix [odd? nil even? odd? nil?] :in-any-order)
  ( (has-prefix [1 2] :in-any-order) [1 3]) => falsey
  ( (has-prefix [2 2] :in-any-order) [2]) => falsey ; duplicates matter

  "seqs"
  (range 33) => (has-prefix [0 1] :in-any-order)
  (range 33) => (has-prefix [1 0 2] :in-any-order)
  (range 33) => (has-prefix (range 3) :in-any-order)

  "When sets prefix sequentials, order is irrelevant"
  [1 2 3 5] => (has-prefix #{2 1 3} :in-any-order)
  ( (has-prefix #{2 1 5} :in-any-order) [1 2 3]) => falsey

  "maps"
  ((has-prefix :a :in-any-order) { :a 1 }) => falsey 
  ((has-prefix :a :in-any-order) [{ :a 1 }]) => falsey
  ((has-prefix [:a] :in-any-order) {:a 1 }) => falsey

  "sets"
  ((has-prefix :a :in-any-order) #{:a}) => falsey
  ((has-prefix #{:a} :in-any-order) #{:a 1}) => falsey
  ((has-prefix [:a] :in-any-order) #{:a 1}) => falsey

  "mixtures"
  [1 2 4] => (has-prefix '(1) :in-any-order)
  '(3 b 1) => (has-prefix ['b 3] :in-any-order)

  [ {:a 1} {:b 1}       ] => (has-prefix [ {:a 1} ] :in-any-order)
  [ {:a 1} {:b 1} {:c 1}] => (has-prefix [ {:b 1} {:a 1} ] :in-any-order)
  [ {:a 1} "irrelevant" ] => (has-prefix   {:a 1} :in-any-order)

  ( (has-prefix [ {:a 1} ] :in-any-order)  [ {:a 1, :b 1} ]) => falsey  
  ( (has-prefix {:a 1} :in-any-order) [ {:a 2} ]) => falsey
  ( (has-prefix {:a 1} :in-any-order) [ 1 2 3 ]) => falsey
  ( (has-prefix {:a 1} :in-any-order) [ [:a 1] ]) => falsey ; I suppose could arguably be true.

  "strings"
  "ab" => (has-prefix "ab" :in-any-order)
  "ab" => (has-prefix "ba" :in-any-order)
  "acb" => (has-prefix "ca" :in-any-order)
  "ab" => (has-prefix "a" :in-any-order)
  ( (has-prefix "bc" :in-any-order) "abc") => falsey
  ( (has-prefix "ab" :in-any-order) "b") => falsey
  ( (has-prefix "ab" :in-any-order) "a") => falsey

  "strings can match collections, either singly or a a collection of strings"
  ["a" "bc" "c"] => (has-prefix "a" :in-any-order)
  ["1" "1 2" "1 2 3"]  => (has-prefix ["1 2" "1"] :in-any-order)
  ["1" "1 2" "1 2 3"]  => (has-prefix ["1" (contains "12 " :in-any-order)] :in-any-order)

  "regexp"
  ( (has-prefix #"bc" :in-any-order) "bc") => (throws Error)

  "regexps can match collections (though the :in-any-order is redundant"
  ["ac" "bd" "c"] => (has-prefix #".c" :in-any-order)
  ( (has-prefix #".c" :in-any-order) ["bd" "ac" "c"]) => falsey
  '("a" "bc" "cccc") => (has-prefix #"." :in-any-order)
  '("a" "bc" "c") => (has-prefix [#"." #".."] :in-any-order)
  ( (has-prefix [#"." #".."] :in-any-order) '("ab" "b" "cd")) => falsey

  [#"bc" #"c"] => (has-prefix #"bc" :in-any-order)

  "nils"
  [nil] => (has-prefix [nil] :in-any-order)
  [nil nil nil] => (has-prefix [nil nil] :in-any-order)
  [nil "foo"] => (has-prefix nil :in-any-order)
  [nil "foo"] => (has-prefix ["foo" nil] :in-any-order)

  ( (has-prefix [nil] :in-any-order) []) => falsey
  
  "individual elements"
  [1 2 3] => (has-prefix 1 :in-any-order)
  [1 2 3] => (has-prefix odd? :in-any-order)
  [nil nil] => (has-prefix nil :in-any-order)
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

  "sets"
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

  "maps can contain individual entries"
  {:k :v} => (contains [:k :v] :in-any-order)
  {:k :v} => (contains (find {:k :v} :k) :in-any-order)
  ((contains :k :in-any-order) {:k :v}) => (throws Error)

  "lists"
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

  "can contain single elements"
  '(1 2 3) => (contains 3 :in-any-order)

  "vectors"
  [3 2 1] => (contains [1] :in-any-order)
  [1 nil 2 3 nil] => (contains [odd? even? odd? nil? nil] :in-any-order)
  [3 2 1] => (contains [1 2] :in-any-order) 
  ( (contains [2 2] :in-any-order) [2]) => falsey ; duplicates matter

  "seqs"
  (range 33) => (contains (reverse [16 17 18]) :in-any-order)
  (range 33) => (contains (reverse (range 16 3)) :in-any-order)
  (reverse (range 33)) => (contains (range 16 3) :in-any-order)

  [3 2 1] => (contains #{2 1} :in-any-order)
  ( (contains #{2 1 5} :in-any-order) [3 2 1]) => falsey

  "mixtures"
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
  "abc" => (contains "ac" :in-any-order)
  "ab" => (contains "ab" :in-any-order)
  "ab" => (contains "ba" :in-any-order)
  ( (contains "ab" :in-any-order) "a") => falsey
  ( (contains "ab" :in-any-order) 1) => falsey

  "strings can match collections, either singly or a a collection of strings"
  ["a" "bc" "c"] => (contains "bc" :in-any-order)
  ["a" "bc" "c"] => (contains (contains "cb" :in-any-order))
  '("a" "bc" "c") => (contains "bc" :in-any-order)
  ["1" "1 2" "1 2 3"]  => (contains ["1 2" "1" ] :in-any-order)
  ( (contains ["1" "2 1"] :in-any-order) ["1" "1 2" "1 2 3"])  => falsey
  #{"a" "bc" "c"} => (contains "bc" :in-any-order)
  ( (contains "bc" :in-any-order) {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  "regexp"
  ( (contains #"bc" :in-any-order) "abc") => (throws Error)

  "regexps can match expressions"
  ["a" "bc" "c"] => (contains #".c" :in-any-order)
  #{"a" "bc" "c"} => (contains #"b5*c" :in-any-order)
  '("a" "bc" "c") => (contains #".." :in-any-order)
  '("a" "bc" "c") => (contains [#".." #"."] :in-any-order)
  ( (contains #"bc" :in-any-order) {"a" 1, "bc" 2, "c" 3}) => (throws Error)

  [#"a" #"bc" #"c"] => (contains #"bc" :in-any-order)
  [#"a" #"bc" #"c"] => (contains [#"bc" #"a"] :in-any-order)

  "sets"
  #{3 2 1} => (contains #{1} :in-any-order)
  #{3 2 1} => (contains #{1 2} :in-any-order)
  #{3 2 1} => (contains [1] :in-any-order)   ; expected needn't be a set
  #{3 2 1} => (contains [1 3] :in-any-order)   ; expected needn't be a set
  ( (contains [1 3] :in-any-order) #{1 2 4}) => falsey
  ( (contains #{1 3} :in-any-order) #{1 2 4}) => falsey
  ( (contains [1 1] :in-any-order) #{1 2 4}) => falsey
  #{3 2 1} => (contains odd? :in-any-order)
  #{3 2 1} => (contains #(= % 1) :in-any-order)
  #{3 2 1} => (contains #{#(= % 1)} :in-any-order)
  ( (contains #{#(= % 1) odd?} :in-any-order) #{2 1}) => falsey

  "nils"
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

  "individual elements"
  [1 2 3] => (contains 2 :in-any-order)
  [1 2 3] => (contains even? :in-any-order)
  #{3 2 1} => (contains even? :in-any-order)
  [nil nil] => (contains nil :in-any-order)
  #{nil 1} => (contains nil :in-any-order)
  )

(def ^{:private true} patches- (ref []))
(defmulti undo-fn :fn)


(defn patches []
 (seq @patches-))

(defn do-patch! [fn & args]
 (dosync
   (apply fn args)
   (let [patch {:fn fn
                :args (vec args)}]
     (alter patches- conj patch)
     patch)))

(defn remove-patch [patch]
  (alter patches- #(remove (fn [p] (= patch p)) %)))

(defn undo-patch [patch]
 (let [fn (undo-fn patch)]
   (dosync
     (fn)
     (remove-patch patch))))

(fact "The patch's undo-fn is called for its side effect and the patch is forgotten"
  (let [visible-evidence-of-a-side-effect (atom nil)]
    (undo-patch ...patch...) => anything
    (provided
      (undo-fn ...patch...) => (fn [] (reset! visible-evidence-of-a-side-effect :happened!))
      (remove-patch ...patch...) => :nothing-of-interest)
    @visible-evidence-of-a-side-effect => :happened!))
	    
