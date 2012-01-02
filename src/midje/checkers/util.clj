;; -*- indent-tabs-mode: nil -*-
(ns midje.checkers.util
  (:use [midje.util.form-utils :only [classic-map?]]
        [midje.util.object-utils :only [name-object]]))

(defn named-as-call
  "Adds a string name that looks like a function call to
   a function's metadata under :name"
  [name expected function]
  (name-object function (format "(%s %s)" name expected)))
  
;; capturedthrowable.clj

(defprotocol ICapturedThrowable
  (throwable [this]))
                       ;; TODO: Alex Jan 1, 2011: rename all this stuff to 'throwable...'
(deftype CapturedThrowable [ex] 
  ICapturedThrowable 
  (throwable [this] ex))

(defn captured-exception [ex] 
  (CapturedThrowable. ex))

(defn captured-exception? [x]
  (satisfies? ICapturedThrowable x))
