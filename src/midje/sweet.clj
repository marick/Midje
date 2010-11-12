(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit
	[midje.unprocessed.background :only [set-background-fakes]]
	[clojure.contrib.pprint :only [pprint]])
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:require [midje.sweet.folded-prerequisites :as folded])
  (:require [me.fogus.unifycle :as unify])
  (:require [midje.sweet.sweet-background :as background])
  (:use midje.sweet.metaconstants)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms])

(defmacro fact [& forms]
  (when (user-desires-checking?)
    (let [runs (folded/rewrite (transform/rewrite (position/add-line-numbers forms)))]
      (define-metaconstants runs)
      `(every? true? (list ~@runs)))))

(defmacro facts [& forms]
  `(fact ~@forms))

(defmacro against-background [description & forms]
  (let [background (background/expand description)]
    `(with-background-fakes ~background ~@forms)))
    
(defmacro background [& description]
  `(set-background-fakes ~(background/expand description)))
