(ns midje.semi-sweet-examples-test
  (:use [midje.semi-sweet] :reload-all)
  (:use [midje.checkers])
  (:use [clojure.test])
  (:use [midje.test-util]))


(def mocked-function)
(defn function-under-test [& rest]
  (apply mocked-function rest))
(defn no-caller [])

(def reported (atom []))

(def reportX) ;; so you can easily see real reports.
(defmacro one-case 
  ([description]
   `(println "Pending:"  ~description))
  ([description during-form & check-forms]
   (let [form-is-during? (fn [form] (and (seq? form)
					 (= (first form) 'during)))]
     (assert (form-is-during? during-form))
     (assert (every? #(not (form-is-during? %)) check-forms))
     `(do 
	(binding [report (fn [report-map#] (swap! reported conj report-map#))]
	  (reset! reported [])
	  ~during-form)
	~@check-forms))))

(defn last-subtype? [expected]
  (= (:subtype (last (deref reported))) expected))
(defn no-failures? []
  (every? #(= (:type %) :pass) (deref reported)))
(defn only-one-failure? []
  (= 1 (count (filter #(= (:type %) :fail) (deref reported)))))
  

(deftest simple-examples
  (one-case "mocked functions must be declared before use")

  (one-case "successful mocking"
    (during (function-under-test) => 33
	    [ (fake (mocked-function) => 33) ])
    (is (no-failures?)))


  (one-case "mocked calls go fine, but function under test produces the wrong result"
     (during (function-under-test 33) => 12
	     [ (fake (mocked-function 33) => (not 12) ) ])
     (is (= (:actual (last @reported)) '(not (clojure.core/= false 12)))))

  (one-case "mock call supposed to be made, but wasn't (zero call count)"
    (during (no-caller) => "irrelevant"
	    [ (fake (mocked-function) => 33) ])
    (is (last-subtype? :incorrect-call-count))
    (is (only-one-failure?)))


  (def other-function)
  (one-case "call not from inside function"
     (during (+ (mocked-function 12) (other-function 12)) => 12
	     [ (fake (mocked-function 12) => 11)
	       (fake (other-function 12) => 1) ])
     (is (no-failures?)))

  (one-case "call that matches none of the expected arguments"
     (during (+ (mocked-function 12) (mocked-function 33)) => "result irrelevant because of earlier failure"
	     [ (fake (mocked-function 12) => "hi") ])
     (is (last-subtype? :unexpected-call))
     (is (only-one-failure?)))

  (one-case "failure because one variant of multiply-mocked function is not called"
     (during (+ (mocked-function 12) (mocked-function 22)) => 3
	     [ (fake (mocked-function 12) => 1)
	       (fake (mocked-function 22) => 2)
	       (fake (mocked-function 33) => 3)])
     (is (last-subtype? :incorrect-call-count))
     (is (only-one-failure?)))

  (one-case "multiple calls to a mocked function are perfectly fine")
)
