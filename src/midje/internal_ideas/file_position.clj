;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.file-position
  (:use [midje.util.zip :only [skip-to-rightmost-leaf]]
        [midje.util.form-utils :only [translate-zipper]]
        [midje.util.namespace :only [namespacey-match]]
        [midje.ideas.arrows :only [all-arrows at-arrow__add-key-value-to-end__no-movement]])
  (:require [clojure.zip :as zip]))

;; COMPILE-TIME POSITIONS.
;; For annotating forms with information retrieved at runtime.
;; For reporting syntax errors

(def fallback-line-number (atom 0))

(defn set-fallback-line-number-from [form]
  (reset! fallback-line-number (or (:line (meta form)) 0)))

(defn- raw-arrow-line-number [arrow-loc]
  (try
      (or (-> arrow-loc zip/left zip/node meta :line)
          (-> arrow-loc zip/right zip/node meta :line)
          (inc (-> arrow-loc zip/prev zip/left zip/node meta :line)))
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





(defn- replace-loc-line [loc loc-with-line]
  (let [m (fn [loc] (meta (zip/node loc)))
        transferred-meta (if (contains? (m loc-with-line) :line)
                           (assoc (m loc) :line (:line (m loc-with-line)))
                           (dissoc (m loc) :line))]
    (zip/replace loc (with-meta (zip/node loc) transferred-meta))))

(defn form-with-copied-line-numbers [form line-number-source]
  (loop [loc (zip/seq-zip form)
         line-loc (zip/seq-zip line-number-source)]
    (cond (zip/end? line-loc)
          (zip/root loc)

          (zip/branch? line-loc)
          (recur (zip/next (replace-loc-line loc line-loc))
                 (zip/next line-loc))

          ;; the form has a tree in place of a non-tree
          (zip/branch? loc)
            (recur (zip/next
                    (skip-to-rightmost-leaf (zip/down (replace-loc-line loc line-loc))))
                   (zip/next line-loc))

          :else
          (recur (zip/next loc)
                 (zip/next line-loc)))))



(defn at-arrow__add-line-number-to-end__no-movement [number loc]
  (at-arrow__add-key-value-to-end__no-movement
   :position `(line-number-known ~number) loc))

(defn annotate-embedded-arrows-with-line-numbers [form]
  (translate-zipper form
    (fn [loc] (namespacey-match all-arrows loc))
    (fn [loc] (at-arrow__add-line-number-to-end__no-movement (arrow-line-number loc) loc))))

