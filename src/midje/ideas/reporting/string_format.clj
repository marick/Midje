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
  (defmethod report-strings :mock-expected-result-inappropriately-matched [m]
    (list
      (fail-at m)
      (str "    Expected: Anything BUT " (pr-sorted (:expected m)))
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
            (format "       %s => %s" (pr-str form) (pr-sorted value)))))
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
    (when (config/choice :visible-future)
      (list
       (str "\n" (color/note "WORK TO DO") " "
            (when-let [doc (format-nested-descriptions (:description m))] (str (pr-str doc) " "))
            "at " (midje-position-string (:position m))))))

  (defmethod report-strings :mock-argument-match-failure [m]
    (list
      (fail-at m)
      (str "You never said "
        (function-name-or-spewage (:var m))
        " would be needed with these arguments:")
      (str "    " (pr-str (:actual m)))))

  (defmethod report-strings :mock-incorrect-call-count [m]
    (letfn [
      (format-one-failure [fail]
        (let [exp (:expected-count fail)
              act (:actual-count fail)
              msg (cond
                    (and (nil? exp) (zero? act))
                    "[expected at least once, actually never called]"
                  
                    (nil? exp)
                    (cl-format nil "[expected at least once, actually called ~R time~:P]" act)
                  
                    :else 
                    (cl-format nil "[expected :times ~A, actually called ~R time~:P]" exp act))]
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


;;; Formatting Summary of Facts

(defn print-clojure-test-result-lines [result])

(defn print-clojure-test-summary-lines [result])

(defn- print-something-actually-happened-summary-results [result])

(defn print-nothing-was-tried-summary-results [])

(defn print-midje-summary-line [result])

(def ^{:private true} previous-failure-count-atom (atom 0))

(defn previous-failure-count []
  @previous-failure-count-atom)

(defn report-strings-summary
  ([midje-result clojure-test-result]
     (when (pos? (:test-count clojure-test-result))
       (print-clojure-test-result-lines clojure-test-result) 
       (print-clojure-test-summary-lines clojure-test-result)
       (println (color/note ">>> Midje summary:")))
     (reset! previous-failure-count-atom (+ (:fail midje-result)
                                           (or (:fail-count clojure-test-result) 0)))
     (print-midje-summary-line midje-result))
  ([midje-result]
     (report-strings-summary midje-result {:test-count 0})))
    


;; Config to expose to reporting namespace, which it will use to show 
;; reported failures to the user

(def report-strings-format-config
  { :single-fact-fn report-strings 
    :summary-fn report-strings-summary })
