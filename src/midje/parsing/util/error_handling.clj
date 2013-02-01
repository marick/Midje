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

(def ^{:dynamic true} *wrap-count* 0)

;; Note: this could be a macro, but it's easy to
;; get confused by macros assisting in the execution
;; of parsing code that parses macros. Using a thunk
;; seems clearer.
(defn parse-and-catch-failure
  "Execute the parsing function (which should have
   already lexically bound the parts of the form it
   needs). Upon success, return the parsed form. If
   the parser discovers an error, it should `report-error`,
   which stops further execution and returns false.
   In case of some exception, the `form-being-parsed`
   is used to attempt some decent output.

   These functions can be dynamically nested: only the
   outermost one will have to deal with parse errors.

   Note that the parsing code should take care not to let
   unparsed code escape from this function via lazy seqs."
  [form-being-parsed parser]
  (if (pos? *wrap-count*)
    (parser)
    (binding [*wrap-count* (inc *wrap-count*)]
      (try
        (parser)
      (catch javax.xml.soap.SOAPException ex
        false)
      (catch Exception ex
        (emit/fail {:type :exception-during-parsing
                    :macro-form form-being-parsed
                    :stacktrace (user-error-exception-lines ex)
                    :position (form-position form-being-parsed)})
        false)))))
     
          

(defn report-error [form & notes]
  (emit/fail {:type :parse-error
              :notes notes
              :position (form-position form)})
  (throw (new javax.xml.soap.SOAPException)))
