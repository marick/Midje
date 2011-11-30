;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [midje.util.old-clojure-contrib.ns-utils :only [immigrate]])
         
  (:use midje.production-mode
        midje.error-handling.monadic
        midje.util.debugging
        [midje.util.form-utils :only [reader-line-number]]
        [midje.util.exceptions :only [user-error-exception-lines]]
        [midje.internal-ideas.wrapping :only [put-wrappers-into-effect]]
        [midje.internal-ideas.file-position :only [set-fallback-line-number-from]]
        [midje.ideas.tabular :only [tabular*]]
        [midje.ideas.facts :only [midjcoexpand
                                  complete-fact-transformation]])
  (:require [midje.ideas.background :as background])
  (:require midje.checkers)
  (:require [midje.util.report :as report]))

(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

;; Following is required because `intern` doesn't transfer "dynamicity".
(def ^{:dynamic true} *include-midje-checks* *include-midje-checks*)

(intern *ns* 'before #'background/before)
(intern *ns* 'after #'background/after)
(intern *ns* 'around #'background/around)

(defmacro background [& forms]
  (when (user-desires-checking?)
    (put-wrappers-into-effect (background/background-wrappers forms))))

(defmacro against-background [background-forms & foreground-forms]
  (if (user-desires-checking?)
    (midjcoexpand `(against-background ~background-forms ~@foreground-forms))
    `(do ~@foreground-forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (try
      (set-fallback-line-number-from &form)
      (let [[background remainder] (background/separate-background-forms forms)]
        (if (seq background)
          `(against-background ~background (midje.sweet/fact ~@remainder))        	
          (complete-fact-transformation remainder)))
      (catch Exception ex
        `(do
           (clojure.test/report {:type :exceptional-user-error
                                 :macro-form '~&form
                                 :exception-lines '~(user-error-exception-lines ex)
                                 :position (midje.internal-ideas.file-position/line-number-known ~(:line (meta &form)))})
           false)))))

(defmacro facts [& forms]
  (with-meta `(fact ~@forms) (meta &form)))

(defn- future-fact-1 [forms]
  (let [lineno (reader-line-number forms)
        description (if (string? (second forms))
                      (str (second forms) " ")
                      "")]
    `(clojure.test/report {:type :future-fact
                           :description ~description
                           :position (midje.internal-ideas.file-position/line-number-known ~lineno)})))

(defmacro future-fact [& forms] (future-fact-1 &form))
(defmacro future-facts [& forms] (future-fact-1 &form))
(defmacro pending-fact [& forms] (future-fact-1 &form))
(defmacro pending-facts [& forms] (future-fact-1 &form))
(defmacro incipient-fact [& forms] (future-fact-1 &form))
(defmacro incipient-facts [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-fact [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-facts [& forms] (future-fact-1 &form))

(defmacro tabular [& _]
  (tabular* (keys &env) &form))