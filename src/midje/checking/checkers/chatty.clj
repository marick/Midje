(ns ^{:doc "Checkers that explain more about a failure."}
  midje.checking.checkers.chatty
  (:use midje.clojure.core
        midje.checking.core
        [midje.checking.checkers.util :only [named-as-call]]
        [midje.checking.checkers.defining :only [as-checker]]
        [midje.parsing.util.core :only [quoted?]]))

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
    (pred-cond arg-form
               (every-pred-m vector? snd-to-last-is-as?) [arg-form (last arg-form)]
               vector?                                   [(-> arg-form (conj :as) (conj as-symbol)) as-symbol]
               (every-pred-m map? has-key-as?)           [arg-form (:as arg-form)]
               map?                                      [(assoc arg-form :as as-symbol) as-symbol]
               :else                                     [arg-form arg-form] )))

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
  (let [result-symbol (gensym "chatty-intermediate-results-")
        [complex-forms substituted-args] (chatty-untease result-symbol args)
        [arg-form arg-name] (single-destructuring-arg->form+name actual-arg)]
    `(as-chatty-checker
      (fn [~arg-form]
        (let [~result-symbol (vec ~complex-forms)]
          (if (extended-false? (~f ~@substituted-args))
            (let [pairs# (vertical-slices '~complex-forms ~result-symbol)]
              (as-data-laden-falsehood {:actual ~arg-name
                                        :intermediate-results pairs#}))
            true))))))
