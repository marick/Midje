(ns midje.semi-sweet-examples-test
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.checkers])
  (:use [clojure.test])
  (:use [midje.test-util]))


(defn mocked-function [] "supposed to be mocked")
(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(def reported (atom []))

(def reportX) ;; so you can easily see real reports.
(defmacro one-case 
  ([description]
   `(println "Pending:"  ~description))
  ([description expect-form & check-forms]
   (let [form-is-expect? (fn [form] (and (seq? form)
					 (= (first form) 'expect)))]
     (assert (form-is-expect? expect-form))
     (assert (every? #(not (form-is-expect? %)) check-forms))
     `(do 
	(binding [report (fn [report-map#] (swap! reported conj report-map#))]
	  (reset! reported [])
	  ~expect-form)
	~@check-forms))))

(defn last-type? [expected]
  (= (:type (last (deref reported))) expected))
(defn no-failures? []
  (every? #(= (:type %) :pass) (deref reported)))
(defn only-one-result? []
  (= 1 (count (deref reported))))
  

(deftest foo
(expect (function-under-test 33) => 12
	[ (fake (mocked-function 33) => (not 12) ) ])
)



(deftest simple-examples
  (one-case "Without expectations, this is just a different syntax for 'is'"
    (expect (function-under-test) => nil))

  (one-case "mocked functions must be declared before use")


  (one-case "successful mocking"
    (expect (function-under-test) => 33
	    [ (fake (mocked-function) => 33) ])
    (is (no-failures?)))


  (one-case "mocked calls go fine, but function under test produces the wrong result"
     (expect (function-under-test 33) => 12
	     [ (fake (mocked-function 33) => (not 12) ) ])
     (is (= (:actual (last @reported)) false))
     (is (= (:expected (last @reported)) 12)))

  (one-case "mock call supposed to be made, but wasn't (zero call count)"
    (expect (no-caller) => "irrelevant"
	    [ (fake (mocked-function) => 33) ])
    (is (last-type? :mock-incorrect-call-count))
    (is (only-one-result?)))


  (def other-function)
  (one-case "call not from inside function"
     (expect (+ (mocked-function 12) (other-function 12)) => 12
	     [ (fake (mocked-function 12) => 11)
	       (fake (other-function 12) => 1) ])
     (is (no-failures?)))



  (one-case "call that matches none of the expected arguments"
     (expect (+ (mocked-function 12) (mocked-function 33)) => "result irrelevant because of earlier failure"
	     [ (fake (mocked-function 12) => "hi") ])
     (is (last-type? :mock-argument-match-failure))
     (is (only-one-result?)))



  (one-case "failure because one variant of multiply-mocked function is not called"
     (expect (+ (mocked-function 12) (mocked-function 22)) => 3
	     [ (fake (mocked-function 12) => 1)
	       (fake (mocked-function 22) => 2)
	       (fake (mocked-function 33) => 3)])
     (is (last-type? :mock-incorrect-call-count))
     (is (only-one-result?)))

  (one-case "multiple calls to a mocked function are perfectly fine")
)
