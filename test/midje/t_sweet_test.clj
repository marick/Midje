(ns midje.t-sweet-test
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util]))

(deftest simple-assertion-examples
  (after 
   (fact (+ 1 1) => 3)
   (is (reported? 1 [{:type :mock-expected-result-failure}])))

  (after 
   (facts (+ 10 10) => 20
	  (+ 20 20) => 40)
   (is (no-failures?)))
)

(deftest fact-takes-doc-strings-that-are-ignored
  (facts "this is a doc string"
	 (+ 10 10) => 20
	 "this is another one"
	 (+ 20 20) => 40)
  )

(deftest fact-returns-a-truth-value
  (is (true? (run-silently (fact (+ 1 1) => 2))))
  (is (false? (run-silently (fact (+ 1 2) => 2))))
  (is (false? (run-silently
	       (fact
		(+ 1 2) => 2
		(+ 1 2) => 3)))))

(only-mocked g)
(defn f [n] (g n))
(defn call2 [n m]
  (+ (g n) (g m)))
  

(deftest simple-mocking-examples
  (after
   (fact (f 1) => 33
      (provided (g 1) => 33))
   (is (no-failures?)))


  (after
   (facts 

    (f 1) => 313
      (provided
         (g 1) => 313)
    
    (f 22) => 500
      (provided 
         (g 22) => 500)
    )
   (is (no-failures?)))


  (after 
   (facts 
    (call2 1 2) => 30
      (provided 
         (g 1) => 10
	 (g 2) => 20)
    )
   (is (no-failures?)))

)




(deftest binding-examples
  (let [outer-value 2]
    (fact
     (let [inner-value 3]
       (call2 outer-value inner-value) => 23
       (provided (g outer-value) => (* 10 outer-value)
		 (g inner-value) => inner-value)))))

(defn always-one [x] 1)
(defn g-caller [x] (g x))

(deftest metaconstant-examples
  (fact (always-one ...anything...) => 1)
  (fact (g-caller ...something...) => ...g-value...
	(provided (g-caller ...something...) => ...g-value...))
  )

(deftest overriding-defaults
  (fact (always-one 3) => 3 :expected-result 1))

(defn some-fn [n] n)
(deftest issue-5-test ; http://github.com/marick/Midje/issues/#issue/5
  (doseq [v (range 5)]
    (fact
     (str "It should be the identity for value " v)
     (some-fn v) => v)))

(deftest issue-2-test ; http://github.com/marick/Midje/issues/#issue/2
  (fact
   (expect (always-one 5) => 1 (not-called some-fn))))

(binding [midje.semi-sweet/*include-midje-checks* false]
  (load "sweet_compile_out"))

  
