(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit
	[clojure.contrib.pprint :only [pprint]])
  (:use midje.util.recognizing-forms)
  (:use [midje.background.midjcoexpand :only [midjcoexpand]])
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:require [midje.sweet.folded-prerequisites :as folded])
  (:require [me.fogus.unifycle :as unify])
  (:require [midje.background :as background])
  (:use midje.sweet.metaconstants)
  (:use [clojure.walk :only [macroexpand-all]])
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms])

(defn- expand [forms]
  (loop [expanded []
	 in-progress forms]
    (cond (empty? in-progress)
	  expanded

	  (is-arrow-form? in-progress)
	  (let [content (transform/one-fake-body-content in-progress)]
	    (recur (conj expanded (-> content transform/make-fake background/make-background))
		   (nthnext in-progress (count content))))
	  
	  :else
	  (throw (Error. "This doesn't look like part of a background:" in-progress)))))



(defmacro background [& description]
  (when (user-desires-checking?)
    (define-metaconstants description)
    `(background/set-background-fakes ~(expand description))))

(defmacro against-background [description & forms]
  (cond (user-desires-checking?)
	(do 
	  (define-metaconstants description)
	  (let [background (expand description)]
	    `(background/with-background-fakes ~background ~@forms)))

	:else
	`(do ~@forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (let [[background remainder] (background/separate-fact forms)
	  runs (folded/rewrite (transform/rewrite (position/add-line-numbers remainder)))]
      (define-metaconstants runs)
      `(against-background ~background
			  (every? true? (list ~@runs))))))

(defmacro facts [& forms]
  `(fact ~@forms))

