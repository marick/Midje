;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.monadic
  (:use [clojure.contrib.pprint :only [cl-format]]
        [clojure.contrib.monads]
        [midje.util report file-position form-utils]
        [clojure.test]))

;; My own maybe monad

(defn as-user-error [form]
  (vary-meta form assoc :midje-user-error true))

(defn user-error-form? [form]
  (:midje-user-error (meta form)))

(defn user-error-report-form [form & notes]
  (as-user-error `(report {:type :user-error
                           :notes '~notes
                           :position '~(form-position form)})))

; Maybe monad
(defmonad midje-maybe-m
   "Monad describing form processing with possible failures. Failure
   is represented by any form with metadata :midje-user-error"
   [m-result identity
    m-bind   (fn [mv f] (if (user-error-form? mv) mv (f mv)))
    ])

(defmacro error-let [let-vector & body]
  `(domonad midje-maybe-m [~@let-vector] ~@body))
(defmacro safely [fn & body]
  `( (with-monad midje-maybe-m (m-lift ~(count body) ~fn))
     ~@body))





