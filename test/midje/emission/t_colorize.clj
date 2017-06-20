(ns midje.emission.t-colorize
  (:require [midje.sweet :refer :all]
            [midje.util.ecosystem :refer [getenv on-windows?]]
            [midje.emission.colorize :as color]
            [midje.config :as config]))

(tabular
  (fact "wraps string in ascii color if the user so desires"
    (prerequisites (color/config-choice :colorize) => ?config-choice
                   (getenv "MIDJE_COLORIZE") => ?env-choice)

    (color/init!)
    (?color-fn "string") => ?result)

  ?color-fn     ?env-choice   ?config-choice   ?result

  ;; On windows, the config-choice defaults to false
  color/fail    "FALSE"       false            "string"
  color/fail    "TRUE"        false            "\u001b[31mstring\u001b[0m"
  color/fail    "reverse"     false            "\u001b[41mstring\u001b[0m"
  color/fail     nil          false            "string"

  ;; On Unixoids, the config-choice defaults to true
  color/fail    "FALSE"       true             "string"
  color/fail    "TRUE"        true             "\u001b[31mstring\u001b[0m"
  color/fail    "reverse"     true             "\u001b[41mstring\u001b[0m"
  color/fail     nil          true             "\u001b[31mstring\u001b[0m"

  ;; The user can set the config variable.
  color/fail     nil          :reverse         "\u001b[41mstring\u001b[0m"

  ;; The environment variable takes precedence
  color/fail     "false"      :reverse         "string")


(tabular
  (fact "about the escape strings that apply to different choices"
    (prerequisite (getenv "MIDJE_COLORIZE") => ?env-value)
    (color/init!)
    (?color-fn "string") => ?result)

  ?color-fn     ?env-value        ?result
  color/fail    "FALSE"           "string"
  color/fail    "TRUE"            "\u001b[31mstring\u001b[0m"
  color/fail    "reverse"         "\u001b[41mstring\u001b[0m"

  color/note    "FALSE"           "string"
  color/note    "TRUE"            "\u001b[36mstring\u001b[0m"
  color/note    "reverse"         "\u001b[46mstring\u001b[0m")

;; Reset to user's default colorization.
(color/init!)


(fact "access environment vars only when namespace is loaded, not on each report line"
  (prerequisite (getenv "MIDJE_COLORIZE") => anything :times 0)

  (color/fail "a") => anything
  (color/note "b") => anything
  (color/fail "c") => anything)
