;; -*- indent-tabs-mode: nil -*-

(ns midje.util.exceptions
  (:use midje.util.strings
	[clojure.contrib.str-utils :only [str-join]]))
  

(defn stacktrace-as-strings [ex]
  (map (fn [elt] (.toString elt)) (.getStackTrace ex)))

(defn relevant-strings-from-stacktrace [ex]
  (remove #(re-find #"^clojure." %) (stacktrace-as-strings ex)))

(defn friendly-exception-lines [ex prefix]
  (cons (.toString ex)
	(map #(str prefix %)
	     (relevant-strings-from-stacktrace ex))))

(defn friendly-exception-text [ex prefix]
  (str-join (System/getProperty "line.separator") (friendly-exception-lines ex prefix)))
