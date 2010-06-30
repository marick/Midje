(ns midje.test-util
  (:use [clojure.test])
)

(defmacro testable-privates [namespace & symbols]
  (let [make-form (fn [symbol] `(def ~symbol (intern '~namespace '~symbol)))
	forms (map make-form symbols)]
  `(do ~@forms))
)

(def reported (atom []))

(defmacro one-case 
  ([description]
   `(println "PENDING:"  ~description))
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

(defn raw-report [] (println @reported))
