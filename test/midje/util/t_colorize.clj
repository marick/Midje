(ns midje.util.t-colorize
  (:require [midje.util.colorize :as c])
  (:use midje.util.colorize
        midje.sweet
        [midje.util.ecosystem :only [getenv]]))

(tabular
  (fact "wraps string in ascii color when env variable is not explicitly set to FALSE"
    (do
      (require '[midje.util.colorize :as c] :reload )
      (?color-fn "string")) => ?result
    (provided
      (getenv "MIDJE_COLORIZE") => ?env-var))

  ?color-fn ?env-var ?result
  c/red     "FALSE" "string"
  c/red     "TRUE"  "\u001b[31mstring\u001b[0m"
  c/red      nil    "\u001b[31mstring\u001b[0m"

  c/yellow  "FALSE" "string"
  c/yellow  "TRUE"  "\u001b[33mstring\u001b[0m"
  c/yellow   nil    "\u001b[33mstring\u001b[0m")

(fact "access environment vars only when namespace is loaded"
  (do
    (red "a")
    (yellow "b")
    (red "c")
    (yellow "d")) => anything
  (provided
    (getenv "MIDJE_COLORIZE") => anything :times 0))
