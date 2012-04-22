(ns ^{:doc "Functions for Midje to deal elegantly with exceptions."}
  midje.error-handling.exceptions
  (:use [clojure.string :only [join]]
        [midje.util.ecosystem :only [line-separator]]
        [midje.util.colorize :only [colorize-choice]]))


;;; Creating 

(defn user-error 
  "Used when a user does something off-limits or incompatible"
  [& lines]
  (Error. (join line-separator lines)))


;;; Printing ergonomic stacktraces

(defn- ^{:testable true} stacktrace-as-strings [^Throwable ex]
  (map str (.getStackTrace ex)))

(letfn [(remove-matches [re strings]
          (remove #(re-find re %) strings))]

  (defn- ^{:testable true} without-clojure-strings [all-strings]
    (remove-matches #"^java\.|^clojure\.|^sun\.|^swank\.|^user\$eval" all-strings))

  (defn- ^{:testable true} without-midje-or-clojure-strings [all-strings]
    (remove-matches #"^java\.|^clojure\.|^sun\.|^swank\.|^user\$eval|^midje" all-strings)))

(defn- ^{:testable true} friendly-exception-lines [ex prefix]
  (cons (str ex)
    (map #(str prefix %)
      (without-midje-or-clojure-strings (stacktrace-as-strings ex)))))

(defn user-error-exception-lines [throwable]
  (cons (str throwable)
    (without-clojure-strings (stacktrace-as-strings throwable))))


;; When a fact throws an Exception or Error it gets wrapped
;; in this deftype

(defprotocol ICapturedThrowable
  (throwable [this])
  (friendly-stacktrace [this]))
                       
(deftype CapturedThrowable [ex] 
  ICapturedThrowable 
  (throwable [this] ex)
  (friendly-stacktrace [this]
    (join line-separator (friendly-exception-lines (throwable this) "              "))))

(defn captured-throwable [ex] 
  (CapturedThrowable. ex))

(defn captured-throwable? [x]
  (instance? CapturedThrowable x))

(defn captured-message [ex]
  (.getMessage ^Throwable (throwable ex)))
