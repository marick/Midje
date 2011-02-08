;; -*- indent-tabs-mode: nil -*-

(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
        [clojure.contrib.pprint :only [pprint]]
        [clojure.contrib.seq :only [separate]]
        clojure.contrib.condition)
         
  (:use [midje production-mode metaconstants]
        midje.midje-forms.recognizing
        [midje.midje-forms.translating :only [midjcoexpand replace-wrappers-returning-immediate
                                              forms-to-wrap-around translate-fact-body
                                              add-line-numbers unfold-prerequisites]]
        [midje.midje-forms.dissecting :only [separate-background-forms]]
        [midje.util report debugging thread-safe-var-nesting]
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
    (replace-wrappers-returning-immediate raw-wrappers)))

(defmacro against-background [wrappers & forms]
  (if (user-desires-checking?)
    (midjcoexpand `(against-background ~wrappers ~@forms))
    `(do ~@forms)))
    
(defmacro fact [& forms]
  (when (user-desires-checking?)
    (try
      (handler-case :type
        (set-fallback-line-number-from &form)
        (let [[background remainder] (separate-background-forms forms)]
          (if (empty? background)
            (let [things-to-run (-> remainder
                                    add-line-numbers
                                    translate-fact-body
                                    unfold-prerequisites)]
              (define-metaconstants things-to-run)
              (multiwrap (midjcoexpand `(every? true? (list ~@things-to-run)))
                         (forms-to-wrap-around :facts)))
            `(against-background ~background (midje.sweet/fact ~@remainder))))
        (catch Exception ex
          `(do (clojure.test/report {:type :exceptional-user-error
                                     :macro-form '~&form
                                     :exception-lines '~(user-error-exception-lines ex)
                                     :position (midje.util.file-position/line-number-known ~(:line (meta &form)))})
               false))
        (handle :user-error
          (println "HI"))))))


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


