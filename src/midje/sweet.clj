(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit)
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:require [midje.sweet.chained-fakes :as chained])
  (:use midje.sweet.metaconstants)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms])

(defmacro fact [& forms]
  (let [runs (chained/rewrite (transform/rewrite (position/add-line-numbers forms)))]
    (define-metaconstants runs)
    `(every? true? (list ~@runs)))
    )

(defmacro facts [& forms]
  `(fact ~@forms))

