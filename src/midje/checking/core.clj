(ns ^{:doc "Core ideas underlying all checking"}
  midje.checking.core
  (:use midje.clojure.core)
  ;; TODO: Shouldn't really have this dependency.
  (:require [midje.emission.plugins.util :as names]))

;;; There is a notion of "extended falsehood", in which a false value may be a
;;; map containing information about what went wrong.

(defn data-laden-falsehood? [value]
  (:midje/data-laden-falsehood (meta value)))

(defn as-data-laden-falsehood [value]
  (vary-meta value assoc :midje/data-laden-falsehood true))

(defn data-laden-falsehood-to-map
  "Used for testing Midje itself, this prevents a Midje
   example of the expected creation of a data-laden falsehood
   from being interpreted as a failure."
  [value]
  (with-meta value {}))

(defn extended-false? [value]
  (or (not value)
      (data-laden-falsehood? value)))

(defn extended-true? [value]
  (not (extended-false? value)))

(defn user-friendly-falsehood [value]
  "'downcast' a possible data-laden falsehood into
   `false` if necessary."
  (if (data-laden-falsehood? value)
    false
    value))

;;; There is a notion of "extended equality" that powers the right-hand-side of Midje examples.

(defn evaluate-checking-function
  "Returns a sequence. The first value is either truthy or falsey.
   If falsey, the second value is a map with
   any additional information. (It may be empty.) If the
   result is an exception, the second value contains it under
   the :thrown key."
  [function actual]
  (try
    (let [function-result (function actual)]
      (if (data-laden-falsehood? function-result)
        [false function-result]
        [function-result {}]))
  (catch Throwable ex
    [false {:thrown ex}])))

(defn extended-= [actual expected]
  (try  
    (cond
     (data-laden-falsehood? actual)      actual
     (data-laden-falsehood? expected)    expected
     (extended-fn? expected)             (first (evaluate-checking-function expected actual))
     (every? regex? [actual expected])   (= (str actual) (str expected))
     (regex? expected)                   (re-find expected actual)
     (and (record? actual) (classic-map? expected))   (= (into {} actual) expected)
     :else                               (= actual expected))
    (catch Throwable ex false)))

(defn extended-list-=
  "Element-by-element comparison, using extended-= for the right-hand-side values."
  [actual-args checkers]
  (and (= (count actual-args) (count checkers))
       (every? (partial apply extended-=) (vertical-slices actual-args checkers))))

;;; An element of extended-= is that an actual map cannot match an expected record (or type).
;;; That produces a plain `false` above. If client code wants to be more informative, it
;;; can use these functions.

(defn inherently-false-map-to-record-comparison? [actual expected]
  (and (record? expected)
       (map? actual)
       (not= (class expected) (class actual))))

;; Leaving the args in case we decide to have a more explicit note.
(defn inherently-false-map-to-record-comparison-note [actual expected]
  (str "A record on the right of the arrow means the value on the left must be of the same type."))
