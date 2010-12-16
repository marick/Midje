(ns midje.test-util
  (:use [clojure.test])
  (:use midje.util.checkers)
  (:use [clojure.contrib.string :only [substring?]])
  (:use [clojure.set :only [subset?]])
)

(defmacro testable-privates [namespace & symbols]
  (let [make-form (fn [symbol] `(def ~symbol (intern '~namespace '~symbol)))
	forms (map make-form symbols)]
  `(do ~@forms))
)

(def reported (atom []))

(defmacro run-silently [run-form]
  `(run-without-reporting (fn [] ~run-form)))

(defn run-without-reporting [function] 
  (binding [report (fn [report-map#] (swap! reported conj report-map#))]
    (reset! reported [])
    (function)))

(defn run-and-check [run-form check-forms]
  `(do
     (run-without-reporting (fn [] ~run-form))
     ~@check-forms))

(defmacro one-case 
  ([description]
   `(println "PENDING:"  ~description))
  ([description expect-form & check-forms]
   (let [form-is-expect? (fn [form] (and (seq? form)
					 (= (first form) 'expect)))]
     (assert (form-is-expect? expect-form))
     (assert (every? #(not (form-is-expect? %)) check-forms))
     (run-and-check expect-form check-forms))))


(defmacro after-silently
  [example-form & check-forms]
  (run-and-check example-form check-forms))


(defn reported?
  ([expected-count]
     (reported? expected-count {}))
  ([expected-count expected-maps]
     (if (= expected-count (count @reported))
       (let [one-expected-matches-one-reported?
	     (fn [expected reported] (subset? (set expected) (set reported)))

	     one-expected-matches-any-reported?
	     (fn [expected reported-seq]
	       (some #(one-expected-matches-one-reported? expected %) reported-seq))

	     all-expected-match-some-reported?
	     (fn [expected-seq reported-seq]
	       (every? (fn [expected]
			 (one-expected-matches-any-reported? expected reported-seq))
		       expected-seq))]
	 (if (all-expected-match-some-reported? expected-maps @reported)
	   true
	   (do
	     (println "Expected not a subset of actual.")
	     (println "Expected" expected-maps)
	     (println "Actual" @reported)
	     false)))
       (do
	 (println "Count" (count @reported) "when" expected-count "expected")
	 (println @reported)
	 false))))
  
  
(defn only-one-result? []
  (reported? 1))
(defn no-failures? []
  (every? #(= (:type %) :pass) @reported))

(defn only-passes? [expected-count]
  (cond (not (= (count @reported) expected-count))
	(do 
	  (println "Count" (count @reported) "when" expected-count "expected")
	  (println @reported)
	  false)

	(not (= (count (filter #(= (:type %) :pass) @reported)) expected-count))
	(do
	  (println "Not everything passed.")
	  (println "Actual" @reported)
	  false)

	:else
	true))

(defn raw-report [] (println @reported) true)

(defmacro in-separate-namespace [& forms]
  `(let [old-ns# *ns*]
    (try (in-ns (gensym)) ~@forms
    (finally (in-ns (ns-name old-ns#))))))

;; Kinds of result maps
(def bad-result (contains {:type :mock-expected-result-failure}))
(def pass (contains {:type :pass}))
(def checker-fails (contains {:type :mock-expected-result-functional-failure}))

