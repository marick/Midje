;; -*- indent-tabs-mode: nil -*-

(ns midje.util.file-position
  (:require [clojure.zip :as zip]))

;; COMPILE-TIME POSITIONS.
;; For annotating forms with information retrieved at runtime.
;; For reporting syntax errors

(def fallback-line-number (atom 0))
(defn set-fallback-line-number-from [form]
  (reset! fallback-line-number (or (:line (meta form)) 0)))

(defn- raw-arrow-line-number [arrow-loc]
  (try
    (let [directional (fn [direction-fn]
                        (fn [loc]
                          (-> loc direction-fn zip/node meta :line)))
          left-lineno (directional zip/left)
          right-lineno (directional zip/right)]
      (or (left-lineno arrow-loc)
          (right-lineno arrow-loc)
          (inc (left-lineno (zip/prev arrow-loc)))))
    (catch Throwable ex nil)))
  
(defn arrow-line-number [arrow-loc]
  (let [raw-lineno (raw-arrow-line-number arrow-loc)]
    (if raw-lineno
      (reset! fallback-line-number raw-lineno)
      (swap! fallback-line-number inc))))

(defn arrow-line-number-from-form [form]
  "Form is of the form [ <function-call> => .* ]"
  (-> form zip/seq-zip zip/down zip/right arrow-line-number))

(defn form-position [form]
  (list *file* (:line (meta form))))


;; RUNTIME POSITIONS
;; For reporting information that is not known (was not
;; inserted at runtime).

(defn user-file-position 
  "Guesses the file position (basename and line number) that the user is
   most likely to be interested in if a test fails."
  []
  (second (map #(list (.getFileName %) (.getLineNumber %))
               (.getStackTrace (Throwable.)))))

(defmacro line-number-known 
  "Guess the filename of a file position, but use the given line number."
  [number]
  `[(first (user-file-position)) ~number])


