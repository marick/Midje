;; -*- indent-tabs-mode: nil -*-

(when (= (class clojure.test/report) clojure.lang.MultiFn)
  (eval
   '(do (require 'clojure.test)
        (ns clojure.test)
        (defonce old-report clojure.test/report))))

(ns midje.util.report
  (:use clojure.test
        [midje.util.form-utils :only [flatten-and-remove-nils]]
        [midje.util.exceptions :only [friendly-exception-text]]
        [midje.checkers.util :only [captured-exception? captured-exception-value]]))

(def *renderer* println)

(defn- midje-position-string [position-pair]
  (format "(%s:%s)" (first position-pair) (second position-pair)))

(defmacro with-identity-renderer [& forms]   ; for testing
  `(binding [*renderer* identity] ~@forms))

(defn- attractively-stringified-form [form]
  (let [named-function-name #(and (fn? %) (:name (meta %)))]
    (cond (named-function-name form)
          (format "a function named '%s'" (named-function-name form))

          (captured-exception? form)
          (friendly-exception-text (captured-exception-value form) "              ")

          :else
          (pr-str form))))

(defn- fail-at [m]
  (str "\nFAIL at " (midje-position-string (:position m))))

(defn- indented [lines]
  (map (fn [line] (str "       " line)) lines))

(defmulti report-strings :type)

(defmethod report-strings :mock-argument-match-failure [m]
   (list
    (fail-at m)
    (str "You never said " (:name (meta (:function m))) " would be needed with these arguments:")
    (str "    " (pr-str (:actual m)))))

(defmethod report-strings :mock-incorrect-call-count [m]
   (list
    (fail-at m)
    (str "You claimed the following was needed, but it was never used:")
    (str "    " (:expected m))))

(defmethod report-strings :mock-expected-result-failure [m]
   (list
    (fail-at m)
    (str "    Expected: " (pr-str (:expected m)))
    (str "      Actual: " (attractively-stringified-form (:actual m)))))

(defmethod report-strings :mock-expected-result-functional-failure [m]
  (list
   (fail-at m)
   "Actual result did not agree with the checking function."
   (str "        Actual result: " (attractively-stringified-form (:actual m)))
   (str "    Checking function: " (pr-str (:expected m)))
   (if (:intermediate-results m)
     (cons "    During checking, these intermediate values were seen:"
           (map (fn [[form value]] (str "       " (pr-str form) " => " (pr-str value)))
                (:intermediate-results m))))
   (if (:notes m)
     (cons "    The checker said this about the reason:"
           (indented (:notes m))))))

(defmethod report-strings :future-fact [m]
  (list
   (str "\nWORK TO DO: "
        (:description m)
        (midje-position-string (:position m)))))
  
(defmethod report-strings :user-error [m]
   (list
    (fail-at m)
    (str "    Midje could not understand something you wrote: ")
    (str "      " (:message m))))
  
(defmethod report-strings :exceptional-user-error [m]
   (list
    (fail-at m)
    (str "    Midje caught an exception when translating this form:")
    (str "      " (pr-str (:macro-form m)))
    (indented (:exception-lines m))))
  
(defn render [m]
  (doall (map *renderer* (flatten-and-remove-nils (report-strings m)))))

(defmethod clojure.test/old-report :default [m]
   (inc-report-counter :fail)
   (render m))

(defmethod clojure.test/old-report :future-fact [m]
   (render m))
