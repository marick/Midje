(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit
	[clojure.contrib.pprint :only [pprint]])
  (:use midje.midje-forms.recognizing)
  (:use [midje.midje-forms.translating :only [midjcoexpand replace-wrappers]])
  (:use [midje.midje-forms.dissecting :only [separate-background-forms]])
  (:require [midje.sweet.sweet-to-semi-sweet-rewrite :as transform])
  (:require [midje.sweet.line-number-insertion :as position])
  (:require [midje.sweet.folded-prerequisites :as folded])
  (:use midje.sweet.metaconstants)
  (:use midje.util.thread-safe-var-nesting)
  (:use midje.util.report)
  (:use [midje.util.form-utils :only [reader-line-number]])
  (:use [midje.util.file-position :only [user-file-position]])
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

(defn- future-fact-1 [forms]
  (let [lineno (reader-line-number forms)
	description (if (string? (second forms))
		      (str (second forms) " ")
		      "")]
    `(clojure.test/report {:type :future-fact
			   :description ~description
			   :position (midje.util.file-position/line-number-known ~lineno)})))

(defmacro future-fact [& forms] (future-fact-1 &form))
(defmacro pending-fact [& forms] (future-fact-1 &form))
(defmacro incipient-fact [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-fact [& forms] (future-fact-1 &form))
