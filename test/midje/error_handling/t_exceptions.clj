;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-exceptions
  (:use [midje.error-handling.exceptions]
        [midje.util.colorize :only [colorized? colorizing?]]
	      [midje sweet test-util]))

(fact "colorizes stacktraces by default" 
  (friendly-stacktrace (Exception. "boom")) => colorized?
  (provided (colorizing?) => true))

(fact "black and white stacktraes when colorization turned off" 
  (friendly-stacktrace (Exception. "boom")) =not=> colorized?
  (provided (colorizing?) => false))

(defrecord R [a])

(fact "captured throwables can be recognized"
  (captured-throwable? (captured-throwable (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-throwable? {}) => falsey
  (captured-throwable? (sorted-map :a 3)) => falsey
  (captured-throwable? (R. 1)) => falsey)