;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-exceptions
  (:use [midje.error-handling.exceptions]
        [midje.util.colorize :only [colorize-choice]]
	      [midje sweet test-util]))

(def red "\033[31m")
(def red-bg "\033[41m")

(tabular "colorizes stacktraces by default"
  (fact  
    (friendly-stacktrace (Exception. "boom")) => (has-prefix ?begins-with)
    (provided (colorize-choice) => ?chosen))
  
  ?chosen    ?begins-with
  "TRUE"     red
  "REVERSE"  red   ;; will be red-bg, once I get Phil to make colors configurable in clj-stacktrace
  "FALSE"    "java.lang.Exception: boom")

(defrecord R [a])

(fact "captured throwables can be recognized"
  (captured-throwable? (captured-throwable (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-throwable? {}) => falsey
  (captured-throwable? (sorted-map :a 3)) => falsey
  (captured-throwable? (R. 1)) => falsey)