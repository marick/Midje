;; -*- indent-tabs-mode: nil -*-

(ns midje.util.exceptions
  (:use [clojure.string :only [join]]
        [clj-stacktrace.repl :only [pst+]]))


(def line-separator ^{:private true} (System/getProperty "line.separator"))

(defn user-error [& lines]
  (Error. (join line-separator lines)))

(defn friendly-stacktrace [ex]
  (with-out-str (pst+ ex)))
