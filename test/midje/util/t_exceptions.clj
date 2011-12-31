;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-exceptions
  (:use [midje.util.exceptions]
        [midje.util.colorize :only [colorized? colorizing?]]
	      [midje sweet test-util]))

(fact "colorizes stacktraces by default" 
  (friendly-stacktrace (Exception. "boom")) => colorized?
  (provided (colorizing?) => true))

(fact "black and white stacktraes when colorization turned off" 
  (friendly-stacktrace (Exception. "boom")) =not=> colorized?
  (provided (colorizing?) => false))
