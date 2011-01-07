;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.chatty
  (:use [midje.checkers util]))

;; TODO: It might make sense to split the notion of extended-falsehood out of
;; that of chatty checkers, since there are now other kinds of checkers that
;; generate chatty falsehoods.

(defn tag-as-chatty-falsehood [value]
  (with-meta value {:midje/chatty-checker-falsehood true}))

(defn chattily-false? [value]
  (or (not value)
      (:midje/chatty-checker-falsehood (meta value))))

(defn tag-as-chatty-checker [function]
  (tag-as-checker (vary-meta function merge {:midje/chatty-checker true})))

(defn chatty-falsehood-to-map [value]
  (with-meta value {}))

(defn chatty-checker-falsehood? [value]
  (:midje/chatty-checker-falsehood (meta value)))

(defn add-actual [actual result]
  (if (chatty-checker-falsehood? result)
    (merge result {:actual actual})
    result))
  
(defn chatty-checker? [fn]
  (:midje/chatty-checker (meta fn)))

(defn chatty-worth-reporting-on? [arg]
  (and (or (list? arg) (seq? arg)) ; what started as a list (fn x y) might now be a seq.
       (> (count arg) 0)
       (not (= (first arg) 'clojure.core/quote))))

(defn chatty-untease [result-symbol arglist]
  (loop [ [current-arg & remainder :as arglist] arglist
          complex-forms []
          substituted-arglist []]
    (cond (empty? arglist)
          [complex-forms substituted-arglist]

          (chatty-worth-reporting-on? current-arg)
          (recur remainder (conj complex-forms current-arg)
                 (conj substituted-arglist `(~result-symbol ~(count complex-forms))))

          :else
          (recur remainder complex-forms (conj substituted-arglist current-arg)))))
          
(defmacro chatty-checker
  "Create a function that returns either true or a detailed description of a failure."
  [ [binding-var] [function & arglist] ]
  (let [result-symbol (gensym "chatty-intermediate-results-")
        [complex-forms substituted-arglist] (chatty-untease result-symbol arglist)]
    `(tag-as-chatty-checker
      (fn [~binding-var]
        (let [~result-symbol (vector ~@complex-forms)]
          (if (chattily-false? (~function ~@substituted-arglist))
            (let [pairs# (map vector '~complex-forms ~result-symbol)]
              (tag-as-chatty-falsehood {:actual ~binding-var,
                                        :intermediate-results pairs#}))
            true))))))

