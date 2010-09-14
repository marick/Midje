(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit)
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:use midje.sweet.metavars)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms]) 

(defmacro fact [& forms]
  (define-metavars forms)
  (let [runs (transform/rewrite (position/add-line-numbers forms))]
    `(every? true? (list ~@runs)))
    )

(defmacro facts [& forms]
  `(fact ~@forms))

