(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit
	[clojure.contrib.pprint :only [pprint]])
  (:use midje.util.recognizing-midje-forms)
  (:use [midje.util.translating-midje-forms :only [midjcoexpand replace-wrappers]])
  (:use [midje.util.dissecting-midje-forms :only [separate-background-forms]])
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:require [midje.sweet.folded-prerequisites :as folded])
  (:use midje.sweet.metaconstants)
  (:use midje.util.thread-safe-var-nesting)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms])


(defmacro background [& wrappers]
  (when (user-desires-checking?)
    (replace-wrappers wrappers)
    nil)) ; no-op

(defmacro against-background [wrappers & forms]
  (if (user-desires-checking?)
    (midjcoexpand `(against-background ~wrappers ~@forms))
    `(do ~@forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (let [[background remainder] (separate-background-forms forms)
	  runs (folded/rewrite (transform/rewrite (position/add-line-numbers remainder)))]
      (define-metaconstants runs)
      `(against-background ~background
			  (every? true? (list ~@runs))))))

(defmacro facts [& forms]
  `(fact ~@forms))

