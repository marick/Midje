;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.monadic
  (:use
    [clojure.algo.monads :only [defmonad domonad with-monad m-lift]]
    [clojure.contrib.seq :only [find-first]]
    [clojure.test :only [report]]
    [midje.util.file-position :only [form-position]]))

(defn as-user-error [form]
  (vary-meta form assoc :midje-user-error true))

(defn user-error-form? [form]
  (:midje-user-error (meta form)))

(defn user-error-report-form [form & notes]
  (as-user-error `(report {:type :user-error
                           :notes '~notes
                           :position '~(form-position form)})))

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

(defn spread-error [collection]
  (or (find-first user-error-form? collection) collection))

;; This is a pretty dubious addition. Not using it now - found
;; a better way - but might need it later.
(defmacro with-valid [symbol & body]
  `(let [~symbol (spread-error ~symbol)]
     (if (user-error-form? ~symbol)
       (eval ~symbol)
       (do ~@body))))


(defmulti validate (fn [form] (name (first form))))
(defmethod validate :default [form] (rest form))
