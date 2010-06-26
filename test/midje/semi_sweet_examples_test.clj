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
(defmacro one-case [description during-form & check-forms]
  (let [form-is-during? (fn [form] (and (seq? form)
				    (= (first form) 'during)))]
    (assert (form-is-during? during-form))
    (assert (every? #(not (form-is-during? %)) check-forms))
    `(do 
       (binding [report (fn [report-map#] (swap! reported conj report-map#))]
	 (reset! reported [])
	 ~during-form)
       ~@check-forms)))

(defn last-subtype? [expected]
  (= (:subtype (last (deref reported))) expected))
(defn no-failures? []
  (every? #(= (:type %) :pass) (deref reported)))
  

(deftest example-tests
  (one-case "successful mocking"
    (during (function-under-test) => 33
	    [ (fake (mocked-function) => 33) ])
;    (println (deref reported))
    (is (no-failures?)))

  (one-case "mock call omitted"
    (during (no-caller) => nil
	    [ (fake (mocked-function) => 33) ])
;    (println (deref reported))
    (is (last-subtype? :incorrect-call-count)))
)
