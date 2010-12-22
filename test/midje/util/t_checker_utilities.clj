(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))
(testable-privates midje.util.checkers index-in)

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

(facts "index-in"
  (index-in 5 []) => false
  (index-in 5 [5]) => 0
  (index-in 5 [1 5]) => 1
  (index-in 5 [2 3]) => false
  (index-in 5 [1 2 5 5]) => 2 ; not 3
  (index-in odd? [1 3 3]) => 0

  (index-in nil [5]) => false
  (index-in nil [nil]) => 0)

  ;; Some special cases that let us skip boundary conditions

(facts "unordered comparisons that allow gaps"
  (seq-comparison [] [] :in-any-order :gaps-ok) => {:actual-found []
  						    :actual-missed []
  						    :expected-found []
  						    :expected-missed [] }

  (seq-comparison [1] [] :in-any-order :gaps-ok) => {:actual-found []
  						     :actual-missed [1]
  						     :expected-found []
  						     :expected-missed [] }

  (seq-comparison [] [1] :in-any-order :gaps-ok) => {:actual-found []
  						     :actual-missed []
  						     :expected-found []
  						     :expected-missed [1] }

  (seq-comparison [1] [1] :in-any-order :gaps-ok) => {:actual-found [1]
  						      :actual-missed []
  						      :expected-found [1]
  						      :expected-missed [] }

  (seq-comparison [1 2] [1] :in-any-order :gaps-ok) => {:actual-found [1]
  							:actual-missed [2]
  							:expected-found [1]
  							:expected-missed [] }

  (seq-comparison [1 2 3] [1 3] :in-any-order :gaps-ok) => {:actual-found [1 3]
  							    :actual-missed [2]
  							    :expected-found [1 3]
  							    :expected-missed [] }

  (seq-comparison [1 2 3] [3 1] :in-any-order :gaps-ok) => {:actual-found [3 1]
  							    :actual-missed [2]
  							    :expected-found [3 1]
  							    :expected-missed [] }

  (seq-comparison [] [1] :in-any-order :gaps-ok) => {:actual-found []
  						     :actual-missed []
  						     :expected-found []
  						     :expected-missed [1] }

  (seq-comparison [1 2 3] [odd?] :in-any-order :gaps-ok) => {:actual-found [1]
  							     :actual-missed [2 3]
  							     :expected-found [odd?]
  							     :expected-missed [] }

  (seq-comparison [1 2 3] [odd? even?] :in-any-order :gaps-ok) => {:actual-found [1 2]
  								   :actual-missed [3]
  								   :expected-found [odd? even?]
  								   :expected-missed [] }

  (seq-comparison [1 2 3] [even? odd?] :in-any-order :gaps-ok) => {:actual-found [2 1]
  								   :actual-missed [3]
  								   :expected-found [even? odd?]
  								   :expected-missed [] }

  (seq-comparison [1 'h 2 'j nil 3 4 5] [even? odd? 3 odd? nil nil odd? even?]
  		  :in-any-order :gaps-ok)
  => {:actual-found [2 1 3 5 nil 4]
      :actual-missed ['h 'j]
      :expected-found [even? odd? 3 odd? nil even?]
      :expected-missed [nil odd?] }

  (seq-comparison [1 2 3] [odd? even? even?] :in-any-order :gaps-ok)
  => {:actual-found [1 2]
      :actual-missed [3]
      :expected-found [odd? even?]
      :expected-missed [even?] }

  (seq-comparison [nil] [] :in-any-order :gaps-ok) => {:actual-found []
  						       :actual-missed [nil]
  						       :expected-found []
  						       :expected-missed [] }

  (seq-comparison [] [nil] :in-any-order :gaps-ok) => {:actual-found []
  						       :actual-missed []
  						       :expected-found []
  						       :expected-missed [nil] }

  (seq-comparison [1 2 3 nil] [odd? nil even?] :in-any-order :gaps-ok) => {:actual-found [1 nil 2]
  									   :actual-missed [3]
  									   :expected-found [odd? nil even?]
  									   :expected-missed [] }

  (seq-comparison ["1" "12"] ["12"] :in-any-order :gaps-ok) => {:actual-found ["12"]
  								:actual-missed ["1"]
  								:expected-found ["12"]
  								:expected-missed [] }

  (seq-comparison ["23" "12"] [21 2] :in-any-order :gaps-ok) => {:actual-found []
  								 :actual-missed ["23" "12"]
  								 :expected-found []
  								 :expected-missed [21 2] }

  ;; :unorderedness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown.
  (seq-comparison ["23" "12"] ["21" "2"] :gaps-ok :in-any-order)
  => {:actual-found []
      :actual-missed ["23" "12"]
      :expected-found []
      :expected-missed ["21", "2"] }

  (let [contains-unordered-21 (contains "21" :in-any-order)
  	contains-2 (contains "2")]
    (seq-comparison ["23" "12"] [contains-unordered-21 contains-2] :in-any-order :gaps-ok)
    => {:actual-found ["23", "12"],
  	:actual-missed []
  	:expected-found [contains-unordered-21 contains-2]
  	:expected-missed [] })
  )


(facts "unordered comparisons that do not allow gaps"
  (seq-comparison []  [] :in-any-order)
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [1] [] :in-any-order)
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [] [1] :in-any-order)
  => (contains {:actual-found [] :expected-found [] :expected [1]})
  (seq-comparison [1] [1] :in-any-order)
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1] :in-any-order)
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1 2] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })
  (seq-comparison [1 2] [2 1] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  ;; Gets the best match, even though a complete match can immediately
  ;; be seen to be impossible.
  (seq-comparison ['x 'y 1 2 3] [2 3 4] :in-any-order)
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3 4]})

  (seq-comparison [1 2] [1 3] :in-any-order)
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 3] :in-any-order)
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 2] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })

  (seq-comparison [1 2 3] [2 1] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  (seq-comparison [1 2 3] [2 3] :in-any-order)
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3] })

  (seq-comparison [1 2 3] [3 2] :in-any-order) =>
  (contains {:actual-found [2 3] :expected-found [2 3] :expected [3 2] })

  (seq-comparison [1 2 3] [odd?] :in-any-order)
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd?] })

  (seq-comparison [1 2 3] [odd? even?] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even?]})

  (seq-comparison [1 2 3] [even? odd?] :in-any-order)
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [even? odd?]})

  (seq-comparison [1 'h nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3] ; shorter actual
  		  :in-any-order)
  => (contains {:actual-found [nil 3 4 5]
 		:expected-found [nil odd? even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})

  (seq-comparison [3 1 'h 2 3 'j nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3]
  		  :in-any-order)
  => (contains {:actual-found [nil 3 4 5]
 		:expected-found [nil odd? even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})


 (seq-comparison [1 2 3] [odd? even? even?] :in-any-order)
 => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even? even?]})

 (seq-comparison [nil] [] :in-any-order)
 => (contains {:actual-found [] :expected-found [] :expected [] })

 (seq-comparison [] [nil] :in-any-order)
 => (contains {:actual-found [] :expected-found [] :expected [nil] })

 (seq-comparison [1 2 3 nil] [odd? nil even?] :in-any-order)
 => (contains {:actual-found [2 3 nil]
 	       :expected-found [even? odd? nil]
 	       :expected [odd? nil even?] })

  (seq-comparison ["1" "12"] ["12"] :in-any-order)
  => (contains {:actual-found ["12"] :expected-found ["12"] :expected ["12"] })

  (seq-comparison ["23" "12"] [21 2] :in-any-order)
  => (contains {:actual-found [] :expected-found [] :expected [21 2] })

  ;; :unorderedness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown.
  (seq-comparison ["23" "12"] ["21" "2"] :in-any-order)
  => (contains {:actual-found []
 		:expected-found []
 		:expected ["21", "2"] })

  (let [contains-unordered-21 (contains "21" :in-any-order)
  	contains-2 (contains "2")]
    (seq-comparison ["23" "12"] [contains-unordered-21 contains-2] :in-any-order)
    => (contains {:actual-found ["23", "12"],
  	:expected-found [contains-unordered-21 contains-2]
 		  :expected [contains-unordered-21 contains-2] }))

  (seq-comparison [1 3] [odd? 1] :in-any-order) =>
  (contains {:actual-found [1 3] :expected-found [1 odd?] :expected [odd? 1] })

  (defn lt2 [x] (< x 2))  ;; Used because (!= #(fn) #(fn) )
  (defn lt3 [x] (< x 3))
  (defn lt4 [x] (< x 4))
  (seq-comparison [1 3] [odd? lt2] :in-any-order)
  => (contains {:actual-found [1 3] :expected-found [lt2 odd?] :expected [odd? lt2] })

  (seq-comparison [1 2 3] [lt2 lt4 lt3] :in-any-order)
  => (contains {:actual-found [1 2 3]
 	     :expected-found [lt2 lt3 lt4]
 	     :expected [lt2 lt4 lt3] })
)

