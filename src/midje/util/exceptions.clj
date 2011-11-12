;; -*- indent-tabs-mode: nil -*-

(ns midje.util.exceptions
  (:use [clojure.string :only [join]]))


;;; Writing

(def line-separator (System/getProperty "line.separator"))

(defn user-error [& lines]
  (Error. (join line-separator lines)))


;;; Reading

(defn stacktrace-as-strings [ex]
  (map #(.toString %) (.getStackTrace ex)))

(defn remove-matches [re strings] 
  (remove #(re-find re %) strings))

(defn without-clojure-strings [all-strings]
  (remove-matches #"^java\.|^clojure\.|^sun\.|^swank\.|^user\$eval" all-strings))
  
(defn without-midje-or-clojure-strings [all-strings]
  (remove-matches #"^java\.|^clojure\.|^sun\.|^swank\.|^user\$eval|^midje" all-strings))

(defn user-error-exception-lines [ex]
  (cons (.toString ex)
        (without-clojure-strings (stacktrace-as-strings ex))))

(defn friendly-exception-lines [ex prefix]
  (cons (.toString ex)
    (map #(str prefix %)
      (without-midje-or-clojure-strings (stacktrace-as-strings ex)))))

(defn friendly-exception-text [ex prefix]
  (join line-separator (friendly-exception-lines ex prefix)))
