;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
        [clojure.contrib.pprint :only [pprint]]
        [clojure.contrib.seq :only [separate]])
         
  (:use [midje production-mode metaconstants]
        midje.midje-forms.recognizing
        [midje.midje-forms.translating
         :only [midjcoexpand put-wrappers-into-effect
                forms-to-wrap-around translate-fact-body
                add-line-numbers unfold-prerequisites
                form-with-copied-line-numbers
                add-binding-annotations]]
        [midje.fakes :only [background-fakes]]
        [midje.midje-forms.dissecting :only [separate-background-forms
                                             dissect-fact-table]]
        [midje.util report debugging thread-safe-var-nesting unify]
        [midje.util.exceptions :only [user-error-exception-lines]]
        [midje.util.wrapping :only [multiwrap]]
        [midje.util.form-utils :only [reader-line-number]]
        [midje.util.file-position :only [user-file-position set-fallback-line-number-from]])
  (:require [midje.midje-forms.building :as building])
  (:require midje.checkers)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)
(intern *ns* 'before #'building/before)
(intern *ns* 'after #'building/after)
(intern *ns* 'around #'building/around)

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
        (if (empty? background)
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
            wrapped-expansion)
          `(against-background ~background (midje.sweet/fact ~@remainder))))
      (catch Exception ex
        `(clojure.test/report {:type :exceptional-user-error
                                   :macro-form '~&form
                                   :exception-lines '~(user-error-exception-lines ex)
                                   :position (midje.util.file-position/line-number-known ~(:line (meta &form)))})))))

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


(defmacro tabular [& forms]
  (let [dissected (dissect-fact-table forms)
        substitute (fn [bindings]
                     (subst (:fact-form dissected) bindings))
        numbered (fn [form]
                   (form-with-copied-line-numbers form (:fact-form dissected)))
        expect-forms (map (comp macroexpand numbered substitute)
                          (:binding-maps dissected))
        result (add-binding-annotations expect-forms
                                        (:binding-maps dissected)
                                        (:map-order dissected))]
    `(do ~@result)))


