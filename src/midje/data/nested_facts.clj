(ns ^{:doc "During checking of a fact, this contains pointers to the facts that contain it,
            in outer-to-inner order. The fact itself is the `last` of the sequence."}
  midje.data.nested-facts
  (:use [clojure.string :only [join]]))

;;; Note: this should probably be a lexically-scoped property of the
;;; fact, created at parse time. However, it was created before facts
;;; were values in their own right, and it's not worth
;;; changing. I doubt anyone will ever notice the dynamic scoping.
;;; (Basically, you'd need a fact that calls a function that
;;; contains a fact.)

(def ^{:dynamic true} *fact-context* [])

(defn descriptions
  ([] *fact-context*)
  ([addition] (conj *fact-context* addition)))

(defmacro adds [description & body]
  `(binding [*fact-context* (conj *fact-context* ~description)]
     ~@body))

