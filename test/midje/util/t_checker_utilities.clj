(ns midje.util.t-checker-collection
  (:use [midje.sweet])
  (:use [midje.test-util]))
(testable-privates midje.util.checkers index-in)

;; (facts "wrapping singletons"
;;   "maps"
;;   (singleton-to-be-wrapped? {} {}) => falsey
;;   (singleton-to-be-wrapped? {} #{}) => falsey
;;   (singleton-to-be-wrapped? {} []) => falsey
;;   (singleton-to-be-wrapped? {} [:a 2]) => truthy
;;   (singleton-to-be-wrapped? {} (find {:k :v} :k)) => truthy
;;   (singleton-to-be-wrapped? {} #{}) => falsey
;;   (singleton-to-be-wrapped? {} (seq [:a 2]) => truthy)

;;   "sequentials"
;;   (singleton-to-be-wrapped? [] 1) => truthy
;;   (singleton-to-be-wrapped? '() 1) => truthy
;;   (singleton-to-be-wrapped? {} 1) => falsey
;;   (singleton-to-be-wrapped? (seq []) 1) => truthy 
;;   (singleton-to-be-wrapped? #{} 1) => truthy

;;   (singleton-to-be-wrapped? [] []) => falsey
;;   (singleton-to-be-wrapped? [] {}) => truthy
;;   (singleton-to-be-wrapped? [] #{}) => falsey

;;   "sets"
;;   (singleton-to-be-wrapped? #{} 1) => truthy
;;   (singleton-to-be-wrapped? #{} []) => falsey
;;   (singleton-to-be-wrapped? #{} #{}) => falsey
;;   (singleton-to-be-wrapped? #{} {}) => truthy

;;   "strings"
;;   (singleton-to-be-wrapped? "" "") => falsey
;;   (singleton-to-be-wrapped? [] "") => truthy
;;   (singleton-to-be-wrapped? "" []) => falsey
;;   (singleton-to-be-wrapped? #{} "") => truthy

;;   "regexps"
;;   (singleton-to-be-wrapped? "" #"") => falsey
;;   (singleton-to-be-wrapped? [] #"") => truthy
;;   (singleton-to-be-wrapped? #"" []) => falsey
;;   (singleton-to-be-wrapped? #{} #"") => truthy
;; )


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

(defmulti multimethod-version-of-odd? (fn [x] true))
(defmethod multimethod-version-of-odd? true [x]
   (odd? x))   
  
(facts "unordered comparisons that allow gaps"
  (seq-comparison []  [] [:gaps-ok :in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [1] [] [:gaps-ok :in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [] [1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected [1]})
  (seq-comparison [1] [1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1 2] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })
  (seq-comparison [1 2] [2 1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  ;; Gets the best match, even though a complete match can immediately
  ;; be seen to be impossible.
  (seq-comparison ['x 'y 1 2 3] [2 3 4] [:gaps-ok :in-any-order])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3 4]})

  (seq-comparison [1 2] [1 3] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 3] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 3] :expected-found [1 3], :expected [1 3] })

  (seq-comparison [1 2 3] [3 1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 3] :expected-found [1 3], :expected [3 1] })

  (seq-comparison [1 2 3] [1 2] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })

  (seq-comparison [1 2 3] [2 1] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  (seq-comparison [1 2 3] [2 3] [:gaps-ok :in-any-order])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3] })

  (seq-comparison [1 2 3] [3 2] [:gaps-ok :in-any-order]) =>
  (contains {:actual-found [2 3] :expected-found [2 3] :expected [3 2] })

  (seq-comparison [1 2 3] [odd?] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd?] })

  (seq-comparison [1 2 3] [odd? even?] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even?]})

  (seq-comparison [1 2 3] [even? odd?] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [even? odd?]})

  (seq-comparison [1 2 3] [odd? odd?] [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 3] :expected-found [odd? odd?] :expected [odd? odd?]})

  (seq-comparison [1 2 3] [even? even?] [:gaps-ok :in-any-order])
  => (contains {:actual-found [2] :expected-found [even?] :expected [even? even?]})

  (seq-comparison [3 5 7] [even? odd? odd? 3]
  		  [:gaps-ok :in-any-order])
  => (contains {:actual-found [3 5 7]
 		:expected-found [3 odd? odd?]
 		:expected [even? odd? odd? 3]})

  (seq-comparison [1 'h nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3] ; shorter actual
  		  [:gaps-ok :in-any-order])
  => (contains {:actual-found [1 nil 3 4 5]
 		:expected-found [odd? nil 3 even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})

  (seq-comparison [3 1 'h 2 3 'j nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3]
  		  [:gaps-ok :in-any-order])
  => (contains {:actual-found [3 1 2 3 nil 3 4 5]
 		:expected-found [3 odd? even? odd? nil 3 even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})


 (seq-comparison [1 2 3] [odd? even? even?] [:gaps-ok :in-any-order])
 => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even? even?]})

 (seq-comparison [nil] [] [:gaps-ok :in-any-order])
 => (contains {:actual-found [] :expected-found [] :expected [] })

 (seq-comparison [] [nil] [:gaps-ok :in-any-order])
 => (contains {:actual-found [] :expected-found [] :expected [nil] })

 (seq-comparison [1 2 3 nil] [odd? nil even?] [:gaps-ok :in-any-order])
 => (contains {:actual-found [1 2 nil]
 	       :expected-found [odd? even? nil]
 	       :expected [odd? nil even?] })

 (seq-comparison [1 2 3 nil] [even? odd? nil] [:gaps-ok :in-any-order])
 => (contains {:actual-found [1 2 nil]
 	       :expected-found [odd? even? nil]
 	       :expected [even? odd? nil] })

  (seq-comparison ["1" "12"] ["12"] [:gaps-ok :in-any-order])
  => (contains {:actual-found ["12"] :expected-found ["12"] :expected ["12"] })

  (seq-comparison ["23" "12"] [21 2] [:gaps-ok :in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected [21 2] })

  ;; :unorderedness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown.
  (seq-comparison ["23" "12"] ["21" "2"] [:in-any-order :gaps-ok])
  => (contains {:actual-found []
 		:expected-found []
 		:expected ["21", "2"] })

  (let [contains-unordered-21 (contains "21" :in-any-order)
  	contains-2 (contains "2")]
    (seq-comparison ["23" "12"] [contains-unordered-21 contains-2] [:in-any-order :gaps-ok])
    => (contains {:actual-found ["23", "12"],
  	:expected-found [contains-2 contains-unordered-21]
 		  :expected [contains-unordered-21 contains-2] }))

  (seq-comparison [1 3] [odd? 1] [:in-any-order :gaps-ok]) =>
  (contains {:actual-found [1 3] :expected-found [1 odd?] :expected [odd? 1] })

  (seq-comparison [1 3] [multimethod-version-of-odd? 1] [:in-any-order :gaps-ok]) =>
  (contains {:actual-found [1 3] :expected-found [1 multimethod-version-of-odd?]
 	     :expected [multimethod-version-of-odd? 1] })

  (defn lt2 [x] (< x 2))  ;; Used because (!= #(fn) #(fn) )
  (defn lt3 [x] (< x 3))
  (defn lt4 [x] (< x 4))
  (seq-comparison [1 3] [odd? lt2] [:in-any-order :gaps-ok])
  => (contains {:actual-found [1 3] :expected-found [lt2 odd?] :expected [odd? lt2] })

  (seq-comparison [1 2 3] [lt2 lt4 lt3] [:in-any-order :gaps-ok])
  => (contains {:actual-found [1 2 3]
		:expected-found [lt2 lt3 lt4]
		:expected [lt2 lt4 lt3] })

  (seq-comparison ["12" "1" "123"] [#"3" #"2" #"1"] #{:in-any-order :gaps-ok})
  => (contains {:actual-found ["12" "1" "123"]
		:expected-found (just [#"2" #"1" #"3"])
		:expected (just [#"3" #"2" #"1"]) })
  )


(facts "unordered comparisons that do not allow gaps"
  (seq-comparison []  [] [:in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [1] [] [:in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [] [1] [:in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected [1]})
  (seq-comparison [1] [1] [:in-any-order])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1] [:in-any-order])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1 2] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })
  (seq-comparison [1 2] [2 1] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  ;; Gets the best match, even though a complete match can immediately
  ;; be seen to be impossible.
  (seq-comparison ['x 'y 1 2 3] [2 3 4] [:in-any-order])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3 4]})

  (seq-comparison [1 2] [1 3] [:in-any-order])
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 3] [:in-any-order])
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 2] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })

  (seq-comparison [1 2 3] [2 1] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [2 1] })

  (seq-comparison [1 2 3] [2 3] [:in-any-order])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3] })

  (seq-comparison [1 2 3] [3 2] [:in-any-order]) =>
  (contains {:actual-found [2 3] :expected-found [2 3] :expected [3 2] })

  (seq-comparison [1 2 3] [odd?] [:in-any-order])
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd?] })

  (seq-comparison [1 2 3] [odd? even?] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even?]})

  (seq-comparison [1 2 3] [even? odd?] [:in-any-order])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [even? odd?]})

  (seq-comparison [1 'h nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3] ; shorter actual
  		  [:in-any-order])
  => (contains {:actual-found [nil 3 4 5]
 		:expected-found [nil odd? even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})

  (seq-comparison [3 1 'h 2 3 'j nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3]
  		  [:in-any-order])
  => (contains {:actual-found [nil 3 4 5]
 		:expected-found [nil odd? even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})


 (seq-comparison [1 2 3] [odd? even? even?] [:in-any-order])
 => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even? even?]})

 (seq-comparison [nil] [] [:in-any-order])
 => (contains {:actual-found [] :expected-found [] :expected [] })

 (seq-comparison [] [nil] [:in-any-order])
 => (contains {:actual-found [] :expected-found [] :expected [nil] })

 (seq-comparison [1 2 3 nil] [odd? nil even?] [:in-any-order])
 => (contains {:actual-found [2 3 nil]
 	       :expected-found [even? odd? nil]
 	       :expected [odd? nil even?] })

  (seq-comparison ["1" "12"] ["12"] [:in-any-order])
  => (contains {:actual-found ["12"] :expected-found ["12"] :expected ["12"] })

  (seq-comparison ["23" "12"] [21 2] [:in-any-order])
  => (contains {:actual-found [] :expected-found [] :expected [21 2] })

  ;; :unorderedness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown.
  (seq-comparison ["23" "12"] ["21" "2"] [:in-any-order])
  => (contains {:actual-found []
 		:expected-found []
 		:expected ["21", "2"] })

  (let [contains-unordered-21 (contains "21" :in-any-order)
  	contains-2 (contains "2")]
    (seq-comparison ["23" "12"] [contains-unordered-21 contains-2] [:in-any-order])
    => (contains {:actual-found ["23", "12"],
		  :expected-found [contains-2 contains-unordered-21]
 		  :expected [contains-unordered-21 contains-2] }))

  (seq-comparison [1 3] [odd? 1] [:in-any-order]) =>
  (contains {:actual-found [1 3] :expected-found [1 odd?] :expected [odd? 1] })

  (seq-comparison [1 3] [multimethod-version-of-odd? 1] [:in-any-order]) =>
  (contains {:actual-found [1 3] :expected-found [1 multimethod-version-of-odd?]
	     :expected [multimethod-version-of-odd? 1] })

  (defn lt2 [x] (< x 2))  ;; Used because (!= #(fn) #(fn) )
  (defn lt3 [x] (< x 3))
  (defn lt4 [x] (< x 4))
  (seq-comparison [1 3] [odd? lt2] [:in-any-order])
  => (contains {:actual-found [1 3] :expected-found [lt2 odd?] :expected [odd? lt2] })

  (seq-comparison [1 2 3] [lt2 lt4 lt3] [:in-any-order])
  => (contains {:actual-found [1 2 3]
 	     :expected-found [lt2 lt3 lt4]
 	     :expected [lt2 lt4 lt3] })
)

(facts "ordered comparisons that allow gaps"
  (seq-comparison []  [] [:gaps-ok])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [1] [] [:gaps-ok])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [] [1] [:gaps-ok])
  => (contains {:actual-found [] :expected-found [] :expected [1]})
  (seq-comparison [1] [1] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1 2] [:gaps-ok])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })
  (seq-comparison [1 'gap] [1 2] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [1] :expected [1 2] })
  (seq-comparison ['gap 1] [1 2] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [1] :expected [1 2] })
  (seq-comparison [1 'gap 2] [1 2] [:gaps-ok])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })

  (seq-comparison [1 2] [2 1] [:gaps-ok])
  => (contains {:actual-found [2] :expected-found [2] :expected [2 1] })

  ;; Gets the best match, even though a complete match can immediately
  ;; be seen to be impossible.
  (seq-comparison ['x 'y 1 2 'y 3] [2 3 4] [:gaps-ok])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3 4]})

  (seq-comparison [1 2] [1 3] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [1 3] [:gaps-ok])
  => (contains {:actual-found [1 3] :expected-found [1 3], :expected [1 3] })

  (seq-comparison [1 2 3] [3 1] [:gaps-ok])
  => (contains {:actual-found [3] :expected-found [3], :expected [3 1] })

  ;; Note that there's no requirement that a particular minimal gap be found.
  (seq-comparison [1 3 2 3 2 1] [3 1] [:gaps-ok])
  => (contains {:actual-found [3 1] :expected-found [3 1], :expected [3 1] })

  (seq-comparison [1 2 3] [odd?] [:gaps-ok])
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd?] })

  (seq-comparison [1 2 3] [odd? even?] [:gaps-ok])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even?]})

  (seq-comparison [1 2 3] [even? odd?] [:gaps-ok])
  => (contains {:actual-found [2 3] :expected-found [even? odd? ] :expected [even? odd?]})

  (seq-comparison [1 2 3] [odd? odd?] [:gaps-ok])
  => (contains {:actual-found [1 3] :expected-found [odd? odd?] :expected [odd? odd?]})

  (seq-comparison [1 2 3] [even? even?] [:gaps-ok])
  => (contains {:actual-found [2] :expected-found [even?] :expected [even? even?]})

  (seq-comparison [1 2 3 5 4 4 3] [odd? even? even?] [:gaps-ok])
  => (contains {:actual-found [1 2 4] :expected-found [odd? even? even?] :expected [odd? even? even?] })

  (seq-comparison [3 5 7] [even? odd? odd? 3]
  		  [:gaps-ok])
  => (contains {:actual-found []
 		:expected-found []
 		:expected [even? odd? odd? 3]})

  (seq-comparison [1 'h nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3] ; shorter actual
  		  [:gaps-ok])
  => (contains {:actual-found [4 5]
 		:expected-found [even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})

  (seq-comparison [3 1 'h 2 3 'j nil 3 4 5 nil] [even? odd? 3 odd? nil nil odd? even? 3]
  		  [:gaps-ok])
  => (contains {:actual-found [2 3 3 5 nil]
 		:expected-found [even? odd? 3 odd? nil]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})


 (seq-comparison [1 2 3] [odd? even? even?] [:gaps-ok])
 => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even? even?]})

 (seq-comparison [nil] [] [:gaps-ok])
 => (contains {:actual-found [] :expected-found [] :expected [] })

 (seq-comparison [] [nil] [:gaps-ok])
 => (contains {:actual-found [] :expected-found [] :expected [nil] })

 (seq-comparison [1 2 3 nil] [odd? nil even?] [:gaps-ok])
 => (contains {:actual-found [1 nil]
 	       :expected-found [odd? nil]
 	       :expected [odd? nil even?] })

 (seq-comparison [1 2 3 nil] [even? odd? nil] [:gaps-ok])
 => (contains {:actual-found [2 3 nil]
 	       :expected-found [even? odd? nil]
 	       :expected [even? odd? nil] })

  (seq-comparison ["1" "12"] ["12"] [:gaps-ok])
  => (contains {:actual-found ["12"] :expected-found ["12"] :expected ["12"] })

  (seq-comparison ["23" "12"] [21 2] [:gaps-ok])
  => (contains {:actual-found [] :expected-found [] :expected [21 2] })

  ;; :gappiness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown below.
  (seq-comparison ["1gap2" "12"] ["21" "2"] [:gaps-ok])
  => (contains {:actual-found []
 		:expected-found []
 		:expected ["21", "2"] })

  (let [contains-gappy-12 (contains "12" :gaps-ok)
  	contains-2 (contains "2")]
    (seq-comparison ["1gap2" "12"] [contains-gappy-12 contains-2] [:gaps-ok])
    => (contains {:actual-found ["1gap2", "12"],
		  :expected-found [contains-gappy-12 contains-2]
  		  :expected [contains-gappy-12 contains-2] }))

  (seq-comparison [1 3] [odd? 1] [:gaps-ok]) =>
  (contains {:actual-found [1] :expected-found [odd?] :expected [odd? 1] })

  (seq-comparison [1] [multimethod-version-of-odd? 1] [:gaps-ok]) =>
  (contains {:actual-found [1] :expected-found [multimethod-version-of-odd?]
 	     :expected [multimethod-version-of-odd? 1] })

  (seq-comparison [3 1] [multimethod-version-of-odd? 1] [:gaps-ok]) =>
  (contains {:actual-found [3 1] :expected-found [multimethod-version-of-odd? 1]
 	     :expected [multimethod-version-of-odd? 1] })
  )


(facts "ordered comparisons that do not allow gaps"
  (seq-comparison []  [] [])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [1] [] [])
  => (contains {:actual-found [] :expected-found [] :expected []})
  (seq-comparison [] [1] [])
  => (contains {:actual-found [] :expected-found [] :expected [1]})
  (seq-comparison [1] [1] [])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1] [])
  => (contains {:actual-found [1] :expected-found [1] :expected [1] })
  (seq-comparison [1 2] [1 2] [])
  => (contains {:actual-found [1 2] :expected-found [1 2] :expected [1 2] })
  (seq-comparison [1 'gap] [1 2] [])
  => (contains {:actual-found [1] :expected-found [1] :expected [1 2] })
  (seq-comparison ['gap 1] [1 2] [])
  => (contains {:actual-found [1] :expected-found [1] :expected [1 2] })
  (seq-comparison [1 'gap 2] [1 2] [])
  => (contains {:actual-found [1] :expected-found [1] :expected [1 2] })

  (seq-comparison [1 2] [2 1] [])
  => (contains {:actual-found [2] :expected-found [2] :expected [2 1] })

  ;; Gets the best match, even though a complete match can immediately
  ;; be seen to be impossible.
  (seq-comparison ['x 'y 1 2 'y 2 3] [2 3 4] [])
  => (contains {:actual-found [2 3] :expected-found [2 3] :expected [2 3 4]})

  (seq-comparison [1 2 3] [1 3] [])
  => (contains {:actual-found [1] :expected-found [1], :expected [1 3] })

  (seq-comparison [1 2 3] [3 1] [])
  => (contains {:actual-found [3] :expected-found [3], :expected [3 1] })

  ;; Note that there's no requirement that a particular minimal gap be found.
  (seq-comparison [1 3 2 3 1 2 1] [3 1] [])
  => (contains {:actual-found [3 1] :expected-found [3 1], :expected [3 1] })

  (seq-comparison [1 2 3] [odd?] [])
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd?] })

  (seq-comparison [1 2 3] [odd? even?] [])
  => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even?]})

  (seq-comparison [1 2 3] [even? odd?] [])
  => (contains {:actual-found [2 3] :expected-found [even? odd? ] :expected [even? odd?]})

  (seq-comparison [1 2 3] [odd? odd?] [])
  => (contains {:actual-found [1] :expected-found [odd?] :expected [odd? odd?]})

  (seq-comparison [1 2 3] [even? even?] [])
  => (contains {:actual-found [2] :expected-found [even?] :expected [even? even?]})

  (seq-comparison [1 2 3 5 4 4 3] [odd? even? even?] [])
  => (contains {:actual-found [5 4 4] :expected-found [odd? even? even?] :expected [odd? even? even?] })

  (seq-comparison [3 5 7] [even? odd? odd? 3] [])
  => (contains {:actual-found []
 		:expected-found []
 		:expected [even? odd? odd? 3]})

  (seq-comparison [1 'h nil 3 4 5] [even? odd? 3 odd? nil nil odd? even? 3] []) ; shorter actual
  => (contains {:actual-found [4 5]
 		:expected-found [even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})

  (seq-comparison [3 1 'h 2 3 'j nil 3 4 5 nil] [even? odd? 3 odd? nil nil odd? even? 3]
  		  [])
  => (contains {:actual-found [2 3]
 		:expected-found [even? odd?]
 		:expected [even? odd? 3 odd? nil nil odd? even? 3]})


 (seq-comparison [1 2 3] [odd? even? even?] [])
 => (contains {:actual-found [1 2] :expected-found [odd? even?] :expected [odd? even? even?]})

 (seq-comparison [nil] [] [])
 => (contains {:actual-found [] :expected-found [] :expected [] })

 (seq-comparison [] [nil] [])
 => (contains {:actual-found [] :expected-found [] :expected [nil] })

 (seq-comparison [1 2 3 nil] [odd? nil even?] [])
 => (contains {:actual-found [3 nil]
 	       :expected-found [odd? nil]
 	       :expected [odd? nil even?] })

 (seq-comparison [1 2 3 nil] [even? odd? nil] [])
 => (contains {:actual-found [2 3 nil]
 	       :expected-found [even? odd? nil]
 	       :expected [even? odd? nil] })

  (seq-comparison ["1" "12"] ["12"] [])
  => (contains {:actual-found ["12"] :expected-found ["12"] :expected ["12"] })

  (seq-comparison ["23" "12"] [21 2] [])
  => (contains {:actual-found [] :expected-found [] :expected [21 2] })

  ;; :gappiness is not inherited by individual strings. If you
  ;; want that, you have to use a nested comparison, as shown below.
  (seq-comparison ["1gap2" "12"] ["21" "2"] [])
  => (contains {:actual-found []
 		:expected-found []
 		:expected ["21", "2"] })

  (seq-comparison [1 3] [odd? 1] []) =>
  (contains {:actual-found [1] :expected-found [odd?] :expected [odd? 1] })

  (seq-comparison [1] [multimethod-version-of-odd? 1] []) =>
  (contains {:actual-found [1] :expected-found [multimethod-version-of-odd?]
 	     :expected [multimethod-version-of-odd? 1] })

  (seq-comparison [3 1 {}] [multimethod-version-of-odd? 1] []) =>
  (contains {:actual-found [3 1] :expected-found [multimethod-version-of-odd? 1]
 	     :expected [multimethod-version-of-odd? 1] })
  )
