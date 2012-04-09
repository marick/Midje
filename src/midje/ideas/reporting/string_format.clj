(ns ^{:doc "The default report format. Prints in colorized strings."} 
  midje.ideas.reporting.string-format
  (:use [clojure.pprint :only [cl-format]]
        [midje.util.object-utils :only [function-name function-name-or-spewage named-function?]]
        midje.error-handling.exceptions
        [midje.internal-ideas.fact-context :only [format-nested-descriptions]]
        [clojure.core.match :only [match]]
        [midje.util.form-utils :only [pred-cond]])
  (:require [midje.util.colorize :as color]))



(defn midje-position-string [[filename line-num]]
  (format "(%s:%s)" filename line-num))

(defn- ^{:testable true} attractively-stringified-form [form]
  (pred-cond form
    named-function?     (format "a function named '%s'" (function-name form))
    captured-throwable? (friendly-stacktrace form)
    :else               (pr-str form)))

(letfn [(fail-at [m]
          (let [description (when-let [doc (format-nested-descriptions (:description m))]
            (str (pr-str doc) " "))
                position (midje-position-string (:position m))
                table-substitutions (when-let [substitutions (:binding-note m)]
              (str "With table substitutions: " substitutions))]
            (list
              (str "\n" (color/fail "FAIL") " " description "at " position)
              table-substitutions)))

        (indented [lines]
          (map (partial str "        ") lines))]

  (defmulti report-strings :type)

  (defmethod report-strings :mock-expected-result-failure [m]
    (list
      (fail-at m)
      (str "    Expected: " (pr-str (:expected m)))
      (str "      Actual: " (attractively-stringified-form (:actual m)))))

  (defmethod report-strings :mock-expected-result-inappropriately-matched [m]
    (list
      (fail-at m)
      (str "    Expected: Anything BUT " (pr-str (:expected m)))
      (str "      Actual: " (attractively-stringified-form (:actual m)))))

  (defmethod report-strings :mock-expected-result-functional-failure [m]
    (list
      (fail-at m)
      "Actual result did not agree with the checking function."
      (str "        Actual result: " (attractively-stringified-form (:actual m)))
      (str "    Checking function: " (pr-str (:expected m)))
      (if (:intermediate-results m)
        (cons "    During checking, these intermediate values were seen:"
          (for [[form value] (:intermediate-results m)]
            (format "       %s => %s" (pr-str form) (pr-str value)))))
      (if (:notes m)
        (cons "    The checker said this about the reason:"
          (indented (:notes m))))))

  (defmethod report-strings :mock-actual-inappropriately-matches-checker [m]
    (list
      (fail-at m)
      "Actual result was NOT supposed to agree with the checking function."
      (str "        Actual result: " (attractively-stringified-form (:actual m)))
      (str "    Checking function: " (pr-str (:expected m)))))

  (defmethod report-strings :future-fact [m]
    (list
      (str "\n" (color/note "WORK TO DO") " "
        (when-let [doc (format-nested-descriptions (:description m))] (str (pr-str doc) " "))
        "at " (midje-position-string (:position m)))))

  (defmethod report-strings :mock-argument-match-failure [m]
    (list
      (fail-at m)
      (str "You never said "
        (function-name-or-spewage (:var m))
        " would be needed with these arguments:")
      (str "    " (pr-str (:actual m)))))

  (defmethod report-strings :mock-incorrect-call-count [m]
    (letfn [(format-one-failure [fail]
              (let [msg (match [(:expected-count fail) (:actual-count fail)]
                [nil 0]                  "[expected at least once, actually never called]"
                [nil act] (cl-format nil "[expected at least once, actually called ~R time~:P]" act)
                [exp act] (cl-format nil "[expected :times ~A, actually called ~R time~:P]" exp act))]
                (str "    " (:expected fail) " " msg)))]

      (concat
        (list (fail-at (first (:failures m)))
          "These calls were not made the right number of times:")
        (map format-one-failure (:failures m)))))

  (defmethod report-strings :validation-error [m]
    (list
      (fail-at m)
      (str "    Midje could not understand something you wrote: ")
      (indented (:notes m))))

  (defmethod report-strings :exceptional-user-error [m]
    (list
      (fail-at m)
      (str "    Midje caught an exception when translating this form:")
      (str "      " (pr-str (:macro-form m)))
      (str "      " "This stack trace *might* help:")
      (indented (:stacktrace m)))))