(ns ^{:doc "The default report format. Prints in colorized strings."} 
  midje.ideas.reporting.string-format
  (:use [clojure.pprint :only [cl-format]]
        [gui-diff.internal :only [nested-sort]]
        [midje.util.object-utils :only [function-name function-name-or-spewage named-function?]]
        midje.error-handling.exceptions
        [midje.util.form-utils :only [pred-cond]])
  (:require [midje.util.colorize :as color]
            [midje.config :as config]
            [clojure.string :as str]))


;;; Formatting Single Fact

(defn midje-position-string [[filename line-num]]   ;;;; DEFUNCT
  (format "(%s:%s)" filename line-num))

(defn- pr-sorted [x]
  (pr-str (nested-sort x)))

(defn- ^{:testable true} attractively-stringified-form [form]
  (pred-cond form
    named-function?     (format "a function named '%s'" (function-name form))
    captured-throwable? (friendly-stacktrace form)
    :else               (pr-sorted form)))

(defn format-nested-descriptions
  "Takes vector like [\"about cars\" nil \"sports cars are fast\"] and returns non-nils joined with -'s
   => \"about cars - sports cars are fast\""
  [nested-description-vector]
  (when-let [non-nil (seq (remove nil? nested-description-vector))]
    (str/join " - " non-nil)))


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


(defn report-strings-summary
  ([midje-result clojure-test-result]))

;; Config to expose to reporting namespace, which it will use to show 
;; reported failures to the user

(def report-strings-format-config
  { :single-fact-fn report-strings 
    :summary-fn report-strings-summary })
