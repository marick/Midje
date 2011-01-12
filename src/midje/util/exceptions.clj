;; -*- indent-tabs-mode: nil -*-

(ns midje.util.exceptions
  (:use midje.util.strings
	[clojure.contrib.str-utils :only [str-join]]))
  

(defn stacktrace-as-strings [ex]
  (map (fn [elt] (.toString elt)) (.getStackTrace ex)))

;; Somehow I am at the moment too stupid to make ->> work.
(defn relevant-strings [strings]
  (let [dump (fn [re strings] (remove #(re-find re %) strings))]
    (dump #"^java\."
          (dump #"^clojure\."
                (dump #"^midje" 
                      (dump #"^user\$eval" strings))))))

(defn friendly-exception-lines [ex prefix]
  (cons (.toString ex)
	(map #(str prefix %)
	     (relevant-strings (stacktrace-as-strings ex)))))

(defn friendly-exception-text [ex prefix]
  (str-join (System/getProperty "line.separator") (friendly-exception-lines ex prefix)))
