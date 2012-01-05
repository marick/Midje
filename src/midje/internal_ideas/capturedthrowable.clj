(ns midje.internal-ideas.capturedthrowable
  "When a fact throws an Exception or Error it gets wrapped
   in this deftype")

(defprotocol ICapturedThrowable
  (throwable [this]))
                       
(deftype CapturedThrowable [ex] 
  ICapturedThrowable 
  (throwable [this] ex))

(defn captured-throwable [ex] 
  (CapturedThrowable. ex))

(defn captured-throwable? [x]
  (instance? CapturedThrowable x))