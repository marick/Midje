(ns ^{:doc "Parsing function argument lists"}
  midje.parsing.arglists
  (:use [midje.error-handling.exceptions :only [user-error]]
        clojure.pprint)
  (:require [midje.emission.levels :as levels]
            [midje.config :as config]
            [midje.util.form-utils :as form]))

(defn separate-print-levels [args default]
  (let [[[print-level & extras] non-levels] (form/separate-by levels/valids args)]
    (when (seq extras)
      (throw (user-error "You have extra print level names or numbers.")))
    (dorun (map levels/validate-level! (filter number? args)))
      
    (if print-level
      [[print-level]  print-level   non-levels]
      [[           ]  default       non-levels])))


