(ns midje.parsing.util.error-handling
  "Utility functions dealing with checking or tranforming forms or zippers."
  (:require [slingshot.slingshot :as slingshot]
            [midje.emission.api :as emit]
            [midje.util.exceptions :refer [user-error-exception-lines]]
            [pointer.core :refer [form-position]]))

(def ^{:dynamic true} *wrap-count* 0)
(def bail-out-of-parsing (gensym))

(defn inside-an-error-handling-wrapper? [] (pos? *wrap-count*))

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
  (letfn [(report-exception [ex]
            (emit/fail {:type :exception-during-parsing
                        :macro-form form-being-parsed
                        :stacktrace (user-error-exception-lines ex)
                        :position (form-position form-being-parsed)}))]
    (if (inside-an-error-handling-wrapper?)
      (parser)
      (binding [*wrap-count* (inc *wrap-count*)]
        (slingshot/try+
         (parser)
         (catch (partial = bail-out-of-parsing) _
           false)
         (catch Exception ex
           (report-exception ex)
           false))))))



(defn report-error [form & notes]
  (emit/fail {:type :parse-error
              :notes notes
              :position (form-position form)})
  (slingshot/throw+ bail-out-of-parsing))
