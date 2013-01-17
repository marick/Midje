(ns ^{:doc "How the default emitter reports on failures"}
  midje.emission.plugins.default-failure-lines
  (:use midje.emission.plugins.util)
  (:use [clojure.pprint :only [cl-format pprint]])
  (:require [midje.clojure-test-facade :as ctf]))

(defmulti messy-lines :type)

(defmethod messy-lines :actual-result-did-not-match-expected-value [m]
  (list
   (str "    Expected: " (attractively-stringified-value (:expected-result m)))
   (str "      Actual: " (attractively-stringified-value (:actual m)))))

(defmethod messy-lines :actual-result-should-not-have-matched-expected-value [m]
  (list
   (str "    Expected: Anything BUT " (attractively-stringified-value (:expected-result m)))
   (str "      Actual: " (attractively-stringified-value (:actual m)))))

(defmethod messy-lines :actual-result-did-not-match-checker [m]
    (list
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

(defmethod messy-lines :actual-result-should-not-have-matched-checker [m]
    (list
      "Actual result was NOT supposed to agree with the checking function."
      (str "        Actual result: " (attractively-stringified-value (:actual m)))
      (str "    Checking function: " (:expected-result-form m))))


(defmethod messy-lines :some-prerequisites-were-called-the-wrong-number-of-times [m]
  (letfn [(format-one-failure [fail]
            (let [exp (:expected-count fail)
                  act (:actual-count fail)
                  msg (cond
                       (and (nil? exp) (zero? act))
                       "[expected at least once, actually never called]"
                  
                       :else 
                       (cl-format nil "[expected :times ~A, actually called ~R time~:P]" exp act))]
              (str "    " (:expected-result-form fail) " " msg)))]
    (cons
     "These calls were not made the right number of times:"
     (map format-one-failure (:failures m)))))

(defmethod messy-lines :prerequisite-was-called-with-unexpected-arguments [m]
  (list
   (str "You never said "
        (prerequisite-var-description (:var m))
        " would be called with these arguments:")
   (str "    " (pr-str (:actual m)))))

(defmethod messy-lines :parse-error [m]
  (list
   (str "    Midje could not understand something you wrote: ")
   (indented (:notes m))))

(defmethod messy-lines :exception-during-parsing [m]
  (list
   (str "    Midje caught an exception when translating this form:")
   (str "      " (pr-str (:macro-form m)))
   (str "      " "This stack trace *might* help:")
   (indented (:stacktrace m))))


(defn summarize [failure-map]
  (let [improved-map (merge failure-map
                           {:expected-result-form (sorted-if-appropriate (:expected-result-form failure-map))})]
    (linearize-lines (cons (failure-notice failure-map)
                           (messy-lines improved-map)))))

