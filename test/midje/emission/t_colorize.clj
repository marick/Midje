(ns midje.emission.t-colorize
  (:use midje.sweet
        [midje.util.ecosystem :only [getenv on-windows?]])
  (:require [midje.emission.colorize :as color]
            [midje.config :as config]))



(tabular
    (fact "wraps string in ascii color when env variable is not explicitly set to FALSE"
      (do
        (color/init!)
        (?color-fn "string")) => ?result
      (provided
        (on-windows?) => ?on-windows :times (range)
        (color/colorize-setting) => ?config-var :times (range)
        (getenv "MIDJE_COLORIZE") => ?env-var))

  ?color-fn   ?env-var  ?config-var  ?on-windows     ?result
  color/fail  "FALSE"   nil          false           "string"
  color/fail  "TRUE"    nil          false           "\u001b[31mstring\u001b[0m"
  color/fail  "reverse" nil          false           "\u001b[41mstring\u001b[0m"
  color/fail   nil      nil          false           "\u001b[31mstring\u001b[0m"
  color/fail   nil      nil          true            "string"

  ;Expore priority of config-var vs env var
  color/fail   nil      true         false           "\u001b[31mstring\u001b[0m"
  color/fail   "FALSE"  true         false           "string"
  color/fail   nil      false        false           "string"
  color/fail   "TRUE"   false        false           "\u001b[31mstring\u001b[0m"

  color/note  "FALSE"   nil          false           "string"
  color/note  "TRUE"    nil          false           "\u001b[36mstring\u001b[0m"
  color/note  "reverse" nil          false           "\u001b[46mstring\u001b[0m"
  color/note   nil      nil          false           "\u001b[36mstring\u001b[0m"
  color/note   nil      nil          true            "string")


;; Reset to user's default colorization.
(color/init!)


(fact "access environment vars only when namespace is loaded"
  (do
    (color/fail "a")
    (color/note "b")
    (color/fail "c")
    (color/note "d")) => anything
  (provided
    (getenv "MIDJE_COLORIZE") => anything :times 0))
