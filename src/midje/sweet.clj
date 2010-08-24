(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit)
  (:require [midje.fact-body-transformation :as transform])
  (:use midje.metavars)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms]) 

(defmacro fact [& forms]
  (define-metavars forms)
  (let [runs (transform/rewrite forms)]
    `(every? true? (list ~@runs)))
    )

(defmacro facts [& forms]
  `(fact ~@forms))

