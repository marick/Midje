(ns midje.test-util
  (:use [clojure.test])
  (:use [clojure.contrib.string :only [substring?]])
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


(defmacro after
  ([description]
   `(println "PENDING:"  ~description))
  ([example-form & check-forms]
   (run-and-check example-form check-forms)))
  

(defn last-type? [expected]
  (= (:type (last @reported)) expected))
(defn last-expected? [expected]
  (= (:expected-call (last @reported)) expected))
(defn last-expected-refers-to? [string]
  (substring? string (str (:expected-call (last @reported)))))
(defn last-function? [expected]
  (= (:function (last @reported)) expected))
(defn last-actual? [expected]
  (= (:actual (last @reported)) expected))
(defn last-file? [expected]
  (= (first (:position (last @reported))) expected))
(defn last-line? [expected]
  (= (second (:position (last @reported))) expected))
(defn no-failures? []
  (every? #(= (:type %) :pass) @reported))
(defn only-one-result? []
  (= 1 (count @reported)))

(defn raw-report [] (println @reported) true)

(defmacro deprivatize [ns-name & names] 
  (let [settings (map (fn [name] `(def ~name ((ns-map (find-ns '~ns-name)) '~name)))
		      names)]
    `(do ~@settings)))
	   
