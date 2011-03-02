;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-error-handling-line-numbers
  (:use [midje sweet test-util]
        [midje.error-handling.monadic]))

;; Different kinds of errors in prerequisites

(defn this-file [line]
  (contains {:position ["t_error_handling_line_numbers.clj", line]}))

(defn raw-this-file [line]
  (contains {:position ["t_error_handling_line_numbers.clj", line]}))


(future-facts "Hook up new fake error handling to sweet level"
(unfinished f)
(after-silently
 (fact (f) => 3 (provided ...movie... => (exactly odd?)))
 (fact @reported => (just (this-file 17))))

(after-silently
 (expect (f) => 3 (fake ...movie... => (exactly odd?)))
 (fact @reported => (just (this-file 21))))
 
(let [raw-fake (fake ...movie... => 3) 
      numbered-raw-fake (fake ...movie... => 3
                              :position (midje.util.file-position/line-number-known 5))]
  (fact
    raw-fake => (raw-this-file 24)
    numbered-raw-fake => (raw-this-file 5)))
)

;; Different kinds of errors in facts.

(after-silently 
 (fact (f) =>)
 (future-fact @reported => (just (this-file 35))))
 
