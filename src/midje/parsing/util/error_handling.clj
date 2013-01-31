(ns ^{:doc "Utility functions dealing with checking or tranforming forms or zippers."}
  midje.parsing.util.error-handling
  (:use midje.clojure.core
        [midje.error-handling.exceptions :only [user-error-exception-lines]]
        [midje.parsing.util.file-position :only [form-position]])

  (:require [bultitude.core :as bultitude]
            [midje.emission.api :as emit])
  ;; Because exception subclasses are so hard to create, throw
  ;; one of these to indicate parsing has terminated. Seems safe."
  (:import javax.xml.soap.SOAPException))

(defmacro rescue-parse-failure [form-being-parsed & body]
  (let [ex (gensym)]
    `(try
       ~@body
     (catch javax.xml.soap.SOAPException ~ex
       false)
     (catch Exception ~ex
       (emit/fail {:type :exception-during-parsing
                   :macro-form ~form-being-parsed
                   :stacktrace (user-error-exception-lines ~ex)
                   :position (form-position ~form-being-parsed)})
       false))))



(defn report-error [form & notes]
  (prn (form-position form))
  (prn *ns*)
  (emit/fail {:type :parse-error
              :notes notes
              :position (form-position form)})
  (throw (new javax.xml.soap.SOAPException)))
