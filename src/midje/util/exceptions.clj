(ns ^{:doc "Functions for Midje to deal elegantly with exceptions."}
  midje.util.exceptions
  (:require [clojure.string :refer [join]]
            [io.aviso.exception :as aviso.exception]
            [io.aviso.ansi :as aviso.ansi]
            [midje.util.ecosystem :refer [line-separator]]))


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

(declare caused-by-lines)

(defn- main-exception-lines [ex prefix]
  (cons (str ex)
        (map #(str prefix %)
             (without-midje-or-clojure-strings (stacktrace-as-strings ex)))))

(defn- ^{:testable true} friendly-exception-lines [ex prefix]
  (concat (main-exception-lines ex prefix)
          (caused-by-lines ex prefix)))

(defn- caused-by-lines [^Throwable ex prefix]
  (when-let [cause (.getCause ex)]
    (let [[message & stacktrace] (friendly-exception-lines cause prefix)]
      (concat ["" (str prefix "Caused by: " message)]
              stacktrace))))

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
    (join line-separator (friendly-exception-lines (throwable this) "  "))))

(defn captured-throwable [ex]
  (CapturedThrowable. ex))

(defn captured-throwable? [x]
  (instance? CapturedThrowable x))

(defn captured-message [ex]
  (.getMessage ^Throwable (throwable ex)))

(defn format-exception [throwable]
  (binding [aviso.exception/*traditional* true
            aviso.exception/*fonts*       (merge aviso.exception/*fonts*
                                                 {:message       aviso.ansi/white-font
                                                  :clojure-frame aviso.ansi/white-font
                                                  :function-name aviso.ansi/white-font})]
    (aviso.exception/format-exception throwable)))
