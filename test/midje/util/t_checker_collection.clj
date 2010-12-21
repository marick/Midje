(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))
(testable-privates midje.util.checkers unordered-seq-comparison index-of-actual)

(facts "wrapping singletons"
  "maps"
  (singleton-to-be-wrapped? {} {}) => falsey
  (singleton-to-be-wrapped? {} #{}) => falsey
  (singleton-to-be-wrapped? {} []) => falsey
  (singleton-to-be-wrapped? {} [:a 2]) => truthy
  (singleton-to-be-wrapped? {} (find {:k :v} :k)) => truthy
  (singleton-to-be-wrapped? {} #{}) => falsey
  (singleton-to-be-wrapped? {} (seq [:a 2]) => truthy)

  "sequentials"
  (singleton-to-be-wrapped? [] 1) => truthy
  (singleton-to-be-wrapped? '() 1) => truthy
  (singleton-to-be-wrapped? {} 1) => falsey
  (singleton-to-be-wrapped? (seq []) 1) => truthy 
  (singleton-to-be-wrapped? #{} 1) => truthy

  (singleton-to-be-wrapped? [] []) => falsey
  (singleton-to-be-wrapped? [] {}) => truthy
  (singleton-to-be-wrapped? [] #{}) => falsey

  "sets"
  (singleton-to-be-wrapped? #{} 1) => truthy
  (singleton-to-be-wrapped? #{} []) => falsey
  (singleton-to-be-wrapped? #{} #{}) => falsey
  (singleton-to-be-wrapped? #{} {}) => truthy

  "strings"
  (singleton-to-be-wrapped? "" "") => falsey
  (singleton-to-be-wrapped? [] "") => truthy
  (singleton-to-be-wrapped? "" []) => falsey
  (singleton-to-be-wrapped? #{} "") => truthy

  "regexps"
  (singleton-to-be-wrapped? "" #"") => falsey
  (singleton-to-be-wrapped? [] #"") => truthy
  (singleton-to-be-wrapped? #"" []) => falsey
  (singleton-to-be-wrapped? #{} #"") => truthy
)  

(facts "index-of-actual"
  (index-of-actual 5 []) => false
  (index-of-actual 5 [5]) => 0
  (index-of-actual 5 [1 5]) => 1
  (index-of-actual 5 [2 3]) => false
  (index-of-actual 5 [1 2 5 5]) => 2 ; not 3
  (index-of-actual odd? [1 3 3]) => 0)

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
  (unordered-seq-comparison [1 2 3] [1 3]) => {:actual-found [1 3]
					   :actual-missed [2]
					   :expected-found [1 3]
					   :expected-missed [] }
  (unordered-seq-comparison [1 2 3] [3 1]) => {:actual-found [3 1]
					   :actual-missed [2]
					   :expected-found [3 1]
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
							      :expected-missed [] }
  (unordered-seq-comparison ["1" "12"] ["12"]) => {:actual-found ["12"]
						  :actual-missed ["1"]
						  :expected-found ["12"]
						  :expected-missed [] }
  (unordered-seq-comparison ["23" "12"] [21 2]) => {:actual-found []
						      :actual-missed ["23" "12"]
						      :expected-found []
						      :expected-missed [21 2] }

  ;; :unorderedness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown.
  (unordered-seq-comparison ["23" "12"] ["21" "2"]) => {:actual-found []
  						      :actual-missed ["23" "12"]
  						      :expected-found []
  						      :expected-missed ["21", "2"] }

  (let [contains-unordered-21 (contains "21" :in-any-order)
	contains-2 (contains "2")]
    (unordered-seq-comparison ["23" "12"] [contains-unordered-21 contains-2])
    => {:actual-found ["12", "23"],
	:actual-missed []
	:expected-found [contains-unordered-21 contains-2]
	:expected-missed [] })
  )


(pending-fact "the quick summary of contains"
  [1 2 3] => (contains [1 3])  ; gaps are allowed
  ( (contains [3 1]) [1 2 3]) => falsey ; order matters by default.
  [1 2 3] => (contains [3 1] :in-any-order))

