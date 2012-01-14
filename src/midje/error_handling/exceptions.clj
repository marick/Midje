(ns ^{:doc "Functions for Midje to deal elegantly with exceptions."}
  midje.error-handling.exceptions
  (:use [clojure.string :only [join]]
        [clj-stacktrace.repl :only [pst+ pst]]
        [midje.util.colorize :only [colorize-choice]]))


(defn user-error 
  "Used when a user does something off-limits or incompatible"
  [& lines]
  (Error. (join (System/getProperty "line.separator") lines)))

;; Beautiful, ergonomic stacktraces

(defprotocol FriendlyStacktrace 
  (friendly-stacktrace [this]))

(extend-protocol FriendlyStacktrace
  Throwable
  (friendly-stacktrace [this] 
    (if (= "FALSE" (colorize-choice))
      (with-out-str (pst this))
      (with-out-str (pst+ this)))))


;; When a fact throws an Exception or Error it gets wrapped
;; in this deftype

(defprotocol ICapturedThrowable
  (throwable [this]))
                       
(deftype CapturedThrowable [ex] 
  ICapturedThrowable 
  (throwable [this] ex)

  FriendlyStacktrace
  (friendly-stacktrace [this] (friendly-stacktrace (.throwable this))))

(defn captured-throwable [ex] 
  (CapturedThrowable. ex))

(defn captured-throwable? [x]
  (instance? CapturedThrowable x))