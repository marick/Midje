(ns midje.internal-ideas.capturedthrowable)

(defprotocol ICapturedThrowable
  (throwable [this]))
                       
(deftype CapturedThrowable [ex] 
  ICapturedThrowable 
  (throwable [this] ex))

(defn captured-throwable [ex] 
  (CapturedThrowable. ex))

(defn captured-throwable? [x]
  (satisfies? ICapturedThrowable x))