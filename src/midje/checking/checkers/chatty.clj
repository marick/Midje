(ns ^{:doc "Checkers that explain more about a failure."}
  midje.checking.checkers.chatty
  (:require [such.control-flow :refer [branch-on]]
            [clojure.pprint :as pprint]
            [midje.checking.checkers.defining :refer [as-checker]]
            [midje.checking.core :refer :all]
            [midje.emission.colorize :as colorize]
            [midje.parsing.util.core :refer [quoted?]]
            [such.sequences :as seq]))

;; Note: checkers need to be exported in ../checkers.clj

(defn as-chatty-checker [function]
  (as-checker (vary-meta function assoc :midje/chatty-checker true)))

(defn chatty-checker? [fn]
  (:midje/chatty-checker (meta fn)))

(defn add-actual [actual result]
  (if (data-laden-falsehood? result)
    (assoc result :actual actual)
    result))

(defn chatty-worth-reporting-on? [arg]
  (and (or (list? arg) (seq? arg)) ; what started as a list (fn x y) might now be a seq.
       (pos? (count arg))
       (not (quoted? arg))))

(defn chatty-untease [result-symbol arglist]
  (reduce (fn [[complex-forms substituted-args] current-arg]
              (if (chatty-worth-reporting-on? current-arg)
                [ (conj complex-forms current-arg),
                  (conj substituted-args `(~result-symbol ~(count complex-forms))) ]
                [complex-forms, (conj substituted-args current-arg)]))
      [[] []]
      arglist))

(defn- ^{:testable true} single-destructuring-arg->form+name [arg-form]
  (let [as-symbol          (gensym 'symbol-for-destructured-arg)
        snd-to-last-is-as? #(= :as (second (reverse %)))
        has-key-as?        #(contains? % :as)]
    (branch-on arg-form
               (every-pred vector? snd-to-last-is-as?)   [arg-form (last arg-form)]
               vector?                                   [(-> arg-form (conj :as) (conj as-symbol)) as-symbol]
               (every-pred map? has-key-as?)             [arg-form (:as arg-form)]
               map?                                      [(assoc arg-form :as as-symbol) as-symbol]
               :else                                     [arg-form arg-form] )))

(defn assert-valid-function! [f]
  (when (symbol? f)
    (let [var-form (resolve f)
          metadata (meta var-form)]
      (cond (or (= var-form #'and)
                (= var-form #'or))
            :ok  ;; They can produce prettier output than `every-checker` and `some-checker`.

            (or (:special-form metadata)
                (:macro metadata))
            (throw (Error. ^String (pprint/cl-format nil "~%~A:~%Chatty checkers can't be used with special forms or macros.~%(`and` and `or` are allowed, as a special case.)~%~%"
                                                     (colorize/fail "PARSE ERROR"))))))))


(defmacro chatty-checker
  "Create a function that returns either true or a description of a failure
   that shows the value of subexpressions. For example, consider this:

     (fact 4 => (fn [actual] (< (h actual) (g actual))))

  The failure message only tells you that 4 was a failing value. Contrast
  to the following:

     (fact 4 => (chatty-checker [actual] (< (h actual) (g actual))))

  The failure output will contain the value of (h actual) and (g actual).

  For more, see `(guide chatty-checkers)`.

  Note: if you want your checkers to be `and` or `or` expressions, use
  `every-checker` or `some-checker` in preference to `chatty-checker`.
  "
  [ [actual-arg] [f & args] ]
  (assert-valid-function! f)

  (let [result-symbol (gensym "chatty-intermediate-results-")
        [complex-forms substituted-args] (chatty-untease result-symbol args)
        [arg-form arg-name] (single-destructuring-arg->form+name actual-arg)]
    `(as-chatty-checker
      (fn [~arg-form]
        (let [~result-symbol (vec ~complex-forms)]
          (if (extended-false? (~f ~@substituted-args))
            (let [pairs# (seq/vertical-slices '~complex-forms ~result-symbol)]
              (as-data-laden-falsehood {:actual ~arg-name
                                        :intermediate-results pairs#}))
            true))))))
