;; -*- indent-tabs-mode: nil -*-

(ns midje.test-util
  (:use [clojure.test])
  (:use midje.checkers)
  (:use [midje.util.old-clojure-contrib.string :only [substring?]])
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

(defmacro after-silently
  [example-form & check-forms]
  (run-and-check example-form check-forms))


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
(def inappropriate-equality (contains {:type :mock-expected-result-inappropriately-matched}))
(def inappropriate-checker (contains {:type :mock-actual-inappropriately-matches-checker}))
(def pass (contains {:type :pass}))
(def checker-fails (contains {:type :mock-expected-result-functional-failure}))
(def wrong-call-count (contains {:type :mock-incorrect-call-count}))
(def a-user-error (contains {:type :user-error}))


(defn at-line [line-no form] (with-meta form {:line line-no}))

