;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-error-handling-line-numbers
  (:use [midje sweet error-handling test-util]))

;; Different kinds of errors in prerequisites
;; TODO: It is beyond stupid that there's both a :file-position and :position.
(defn this-file [line]
  (contains {:position ["t_error_handling_line_numbers.clj", line]}))

(defn raw-this-file [line]
  (contains {:file-position ["t_error_handling_line_numbers.clj", line]}))

(unfinished f)
(after-silently
 (fact (f) => 3 (provided ...movie... => (exactly odd?)))
 (fact @reported => (just (this-file 16))))

(after-silently
 (expect (f) => 3 (fake ...movie... => (exactly odd?)))
 (fact @reported => (just (this-file 20))))
 
(let [raw-fake (fake ...movie... => 3) 
      numbered-raw-fake (fake ...movie... => 3
                              :file-position (midje.util.file-position/line-number-known 5))]
  (fact
    raw-fake => (raw-this-file 23)
    numbered-raw-fake => (raw-this-file 5)))


;; Different kinds of errors in facts.

(after-silently 
 (fact (f) =>)
 (fact @reported => (just (this-file 34))))
 
