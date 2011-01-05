;; -*- indent-tabs-mode: nil -*-

(ns midje.util.file-position)

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

