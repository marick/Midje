;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling
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








(defn make-broken-fake [overrides & message-args]
  (merge `{:type :broken-fake
           :message ~(apply (partial cl-format nil) message-args)
          :position (user-file-position) }
         (apply hash-map-duplicates-ok overrides)))

(defn broken-fake? [thing]
  (and (map? thing) (= (:type thing) :broken-fake)))

(defn report-broken-fakes [brokens]
  (doseq [busted brokens]
    (report {:type :user-error
             :message (:message busted)
             :position (:position busted)})))

(defn best-guess-at-overrides [fake-forms]
  (let [ [_ _ _ & overrides] fake-forms] overrides))

(defn broken-fake [forms]
  (cond (not (list? (first forms)))
        (make-broken-fake
         (best-guess-at-overrides forms)
         "Left-hand-side must look like a function call. ~S doesn't."
         (first forms))

        :else
        nil))
    
