(ns ^{:doc "How the default emitter reports on failures"}
  midje.emission.plugins.default-failure-lines
  (:use midje.emission.plugins.util)
  (:use [clojure.pprint :only [cl-format pprint]])
  (:require [midje.clojure-test-facade :as ctf]))

(defmulti messy-lines :type)

(defmethod messy-lines :mock-expected-result-failure [m]
  (list
   (failure-notice m)
   (str "    Expected: " (:expected-result-form m))
   (str "      Actual: " (attractively-stringified-value (:actual m)))))

(defmethod messy-lines :mock-expected-result-inappropriately-matched [m]
  (list
   (failure-notice m)
   (str "    Expected: Anything BUT " (:expected-result-form m))
   (str "      Actual: " (attractively-stringified-value (:actual m)))))

(defmethod messy-lines :mock-expected-result-functional-failure [m]
    (list
      (failure-notice m)
      "Actual result did not agree with the checking function."
      (str "        Actual result: " (attractively-stringified-value (:actual m)))
      (str "    Checking function: " (:expected-result-form m))
      (if (:intermediate-results m)
        (cons "    During checking, these intermediate values were seen:"
          (for [[form value] (:intermediate-results m)]
            (format "       %s => %s" (pr-str form) (attractively-stringified-value value)))))
      (if (:notes m)
        (cons "    The checker said this about the reason:"
          (indented (:notes m))))))

(defmethod messy-lines :mock-actual-inappropriately-matches-checker [m]
    (list
      (failure-notice m)
      "Actual result was NOT supposed to agree with the checking function."
      (str "        Actual result: " (attractively-stringified-value (:actual m)))
      (str "    Checking function: " (:expected-result-form m))))


(defmethod messy-lines :mock-incorrect-call-count [m]
  (letfn [(format-one-failure [fail]
            (let [exp (:expected-count fail)
                  act (:actual-count fail)
                  msg (cond
                       (and (nil? exp) (zero? act))
                       "[expected at least once, actually never called]"
                  
                       :else 
                       (cl-format nil "[expected :times ~A, actually called ~R time~:P]" exp act))]
              (str "    " (:expected-result-form fail) " " msg)))]
    (concat
     (list (failure-notice (first (:failures m)))
           "These calls were not made the right number of times:")
     (map format-one-failure (:failures m)))))

(defmethod messy-lines :mock-argument-match-failure [m]
  (list
   (failure-notice m)
   (str "You never said "
        (prerequisite-var-description (:var m))
        " would be called with these arguments:")
   (str "    " (pr-str (:actual m)))))

(defmethod messy-lines :validation-error [m]
  (list
   (failure-notice m)
   (str "    Midje could not understand something you wrote: ")
   (indented (:notes m))))




(defmethod messy-lines :default [failure-map]
  (midje.ideas.reporting.string-format/report-strings failure-map))

(defn summarize [failure-map]
  (let [improved-map (merge failure-map
                           {:expected-result-form (sorted-if-appropriate (:expected-result-form failure-map))})]
    (linearize-lines (messy-lines improved-map))))

