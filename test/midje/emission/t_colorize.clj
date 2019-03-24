(ns midje.emission.t-colorize
  (:require [midje.sweet :refer :all]
            [midje.util.ecosystem :as ecosystem]
            [midje.emission.colorize :as color]))

(tabular
 (fact "wraps string in ascii color if the user so desires"
    (prerequisite (color/config-choice :colorize) => ?choice)
    (?color-fn "string") => ?result)

  ?color-fn   ?choice   ?result
  color/fail  :false    "string"
  color/fail  :true     "\u001b[31mstring\u001b[0m"
  color/fail  :reverse  "\u001b[41mstring\u001b[0m"

  color/note  :false    "string"
  color/note  :true     "\u001b[36mstring\u001b[0m"
  color/note  :reverse  "\u001b[46mstring\u001b[0m"

  color/pass  :false   "string"
  color/pass  :true    "\u001b[32mstring\u001b[0m"
  color/pass  :reverse "\u001b[42mstring\u001b[0m")

(fact "access environment vars once, not on each report line"
      (color/fail "a") => anything
      (provided
       (ecosystem/getenv "MIDJE_COLORIZE") => anything :times 0)

      (color/note "b") => anything
      (provided
       (ecosystem/getenv "MIDJE_COLORIZE") => anything :times 0)

      (color/fail "c") => anything
      (provided
       (ecosystem/getenv "MIDJE_COLORIZE") => anything :times 0))
