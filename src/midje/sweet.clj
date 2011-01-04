;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
        clojure.contrib.error-kit
        [clojure.contrib.pprint :only [pprint]])
  (:use [midje.production-mode])
  (:use midje.midje-forms.recognizing)
  (:use [midje.midje-forms.translating :only [midjcoexpand replace-wrappers-returning-immediate
                                              forms-to-wrap-around translate-fact-body
                                              add-line-numbers]])
  (:use [midje.midje-forms.dissecting :only [separate-background-forms]])
  (:require [midje.midje-forms.building :as building])
  (:require [midje.sweet.folded-prerequisites :as folded])
  (:use [clojure.contrib.seq :only [separate]])
  (:use midje.metaconstants)
  (:use [midje.util report debugging thread-safe-var-nesting])
  (:use [midje.util.wrapping :only [multiwrap]])
  (:use [midje.util.form-utils :only [reader-line-number]])
  (:use [midje.util.file-position :only [user-file-position]])
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)
(intern *ns* 'before #'building/before)
(intern *ns* 'after #'building/after)
(intern *ns* 'around #'building/around)

(deferror odd-test-forms [] [forms])


(defmacro background [& raw-wrappers]
  (when (user-desires-checking?)
    (replace-wrappers-returning-immediate raw-wrappers)))

(defmacro against-background [wrappers & forms]
  (if (user-desires-checking?)
    (midjcoexpand `(against-background ~wrappers ~@forms))
    `(do ~@forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (let [[background remainder] (separate-background-forms forms)]
      (if (empty? background)
        (let [things-to-run (folded/rewrite
                             (translate-fact-body
                              (add-line-numbers remainder)))]
          (define-metaconstants things-to-run)
          (multiwrap (midjcoexpand `(every? true? (list ~@things-to-run)))
                     (forms-to-wrap-around :facts)))
        `(against-background ~background (midje.sweet/fact ~@remainder))))))


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

;; Wanna add more to these? See also midje-forms.recognizing.
;; Such is the penalty for whimsy.
(defmacro future-fact [& forms] (future-fact-1 &form))
(defmacro future-facts [& forms] (future-fact-1 &form))
(defmacro pending-fact [& forms] (future-fact-1 &form))
(defmacro pending-facts [& forms] (future-fact-1 &form))
(defmacro incipient-fact [& forms] (future-fact-1 &form))
(defmacro incipient-facts [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-fact [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-facts [& forms] (future-fact-1 &form))
