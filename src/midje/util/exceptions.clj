;; -*- indent-tabs-mode: nil -*-

(ns midje.util.exceptions
  (:use [clojure.string :only [join]]
        [clj-stacktrace.repl :only [pst+ pst-str]]
        [midje.util.colorize :only [colorizing?]]))


(def line-separator ^{:private true} (System/getProperty "line.separator"))

(defn user-error [& lines]
  (Error. (join line-separator lines)))

(defn friendly-stacktrace [ex]
  (if (colorizing?)
    (with-out-str (pst+ ex))
    (pst-str ex)))