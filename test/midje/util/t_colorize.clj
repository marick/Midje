(ns midje.util.t-colorize
  (:require [midje.util.colorize :as c])
  (:use midje.util.colorize
        midje.sweet
        [midje.util.ecosystem :only [getenv]]))

(tabular
  (fact "wraps string in ascii color when env variable is not explicitly set to FALSE"
    (do
      (require '[midje.util.colorize :as c] :reload ) ;; enables 'provided' to take
      (?color-fn "string")) => ?result
    (provided
      (getenv "MIDJE_COLORIZE") => ?env-var :times 1))

  ?color-fn ?env-var ?result
  c/fail-color     "FALSE" "string"
  c/fail-color     "TRUE"  "\u001b[31mstring\u001b[0m"
  c/fail-color      nil    "\u001b[31mstring\u001b[0m"

  c/note-color  "FALSE" "string"
  c/note-color  "TRUE"  "\u001b[36mstring\u001b[0m"
  c/note-color   nil    "\u001b[36mstring\u001b[0m")

(fact "access environment vars only when namespace is loaded"
  (do
    (fail-color "a")
    (note-color "b")
    (fail-color "c")
    (note-color "d")) => anything
  (provided
    (getenv "MIDJE_COLORIZE") => anything :times 0))
