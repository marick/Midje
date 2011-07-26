;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]])
         
  (:use [midje production-mode metaconstants]
        [midje.internal-ideas.midjcoexpansion :only [midjcoexpand forms-to-wrap-around]]
        [midje.midje-forms.translating
         :only [put-wrappers-into-effect
                translate-fact-body
                add-line-numbers unfold-prerequisites]]
        [midje.tabular :only [tabular*]]
        [midje.error-handling monadic]
        [midje.background :only [separate-background-forms background-fakes]]
        [midje.util debugging thread-safe-var-nesting unify]
        [midje.util.exceptions :only [user-error-exception-lines]]
        [midje.util.wrapping :only [multiwrap]]
        [midje.util.form-utils :only [reader-line-number]]
        [midje.util.file-position :only [user-file-position set-fallback-line-number-from]])
  (:require [midje.background :as background])
  (:require midje.checkers)
  (:require [midje.util.report :as report])
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)
(intern *ns* 'before #'background/before)
(intern *ns* 'after #'background/after)
(intern *ns* 'around #'background/around)

(defmacro background [& raw-wrappers]
  (when (user-desires-checking?)
    (put-wrappers-into-effect raw-wrappers)))

(defmacro against-background [wrappers & forms]
  (if (user-desires-checking?)
    (midjcoexpand `(against-background ~wrappers ~@forms))
    `(do ~@forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (try
      (set-fallback-line-number-from &form)
      (let [[background remainder] (separate-background-forms forms)]
        (if (seq background)
          `(against-background ~background (midje.sweet/fact ~@remainder))        	
          
          (let [things-to-run (-> remainder
                                  add-line-numbers
                                  translate-fact-body
                                  unfold-prerequisites)
                fake-enabled `(with-installed-fakes
                                (background-fakes)
                                (every? true?
                                        (list ~@things-to-run)))
                expansion (midjcoexpand fake-enabled)
                wrapped-expansion (multiwrap expansion
                                             (forms-to-wrap-around :facts))]
            (define-metaconstants things-to-run)
            `(do (report/fact-begins)
                 ~wrapped-expansion
                 (report/fact-checks-out?)))))
      (catch Exception ex
        `(do
           (clojure.test/report {:type :exceptional-user-error
                                 :macro-form '~&form
                                 :exception-lines '~(user-error-exception-lines ex)
                                 :position (midje.util.file-position/line-number-known ~(:line (meta &form)))})
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
                           :position (midje.util.file-position/line-number-known ~lineno)})))

(defmacro future-fact [& forms] (future-fact-1 &form))
(defmacro future-facts [& forms] (future-fact-1 &form))
(defmacro pending-fact [& forms] (future-fact-1 &form))
(defmacro pending-facts [& forms] (future-fact-1 &form))
(defmacro incipient-fact [& forms] (future-fact-1 &form))
(defmacro incipient-facts [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-fact [& forms] (future-fact-1 &form))
(defmacro antiterminologicaldisintactitudinarian-facts [& forms] (future-fact-1 &form))

(defmacro tabular [& _]
  (tabular* &form))
