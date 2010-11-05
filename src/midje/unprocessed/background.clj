(ns midje.unprocessed.background
  (:use [midje.util.thread-safe-var-nesting])
)

(def *background-fakes* '())

(defn push-background-fakes [fakes] (push-safely #'*background-fakes* fakes))
(defn pop-background-fakes [] (pop-safely #'*background-fakes*))
(defn adding-background-fakes [fakes]
  (flatten (cons fakes *background-fakes*)))

