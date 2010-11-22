(ns midje.background.midjcoexpand
  (:use midje.sweet))

(macroexpand-1 '(fact 1 => 2))

(defn midjcoexpand [form]
  (println form)
  (macroexpand-1 form))

