(ns midje.util.t-colorize
  (:require [midje.util.colorize :as color])
  (:use midje.util.colorize
        midje.sweet
        [midje.util.ecosystem :only [getenv on-windows?]]))

(tabular
  (fact "wraps string in ascii color when env variable is not explicitly set to FALSE"
    (do
      (require '[midje.util.colorize :as color] :reload ) ;; enables 'provided' to take
      (?color-fn "string")) => ?result
    (provided
      (on-windows?) => ?on-windows :times (range)
      (getenv "MIDJE_COLORIZE") => ?env-var))

  ?color-fn   ?env-var      ?on-windows     ?result
  color/fail  "FALSE"       false           "string"
  color/fail  "TRUE"        false           "\u001b[31mstring\u001b[0m"
  color/fail  "reverse"     false           "\u001b[41mstring\u001b[0m"
  color/fail   nil          false           "\u001b[31mstring\u001b[0m"
  color/fail   nil          true            "string"

  color/note  "FALSE"       false           "string"
  color/note  "TRUE"        false           "\u001b[36mstring\u001b[0m"
  color/note  "reverse"     false           "\u001b[46mstring\u001b[0m"
  color/note   nil          false           "\u001b[36mstring\u001b[0m"
  color/note   nil          true            "string")

;; Reset to user's default colorization.
(require '[midje.util.colorize] :reload )


(fact "access environment vars only when namespace is loaded"
  (do
    (color/fail "a")
    (color/note "b")
    (color/fail "c")
    (color/note "d")) => anything
  (provided
    (getenv "MIDJE_COLORIZE") => anything :times 0))



(fact 
  (colorize-deftest-output "FAIL in deftest failure message") 
     => "\u001b[31mFAIL\u001b[0m in deftest failure message")

(fact 
  (colorize-deftest-output "ERROR in deftest failure message") 
     => "\u001b[31mERROR\u001b[0m in deftest failure message")

(fact 
  (colorize-deftest-output "ERROR in deftest failure message ERROR") 
     =not=> "\u001b[31mERROR\u001b[0m in deftest failure message \u001b[31mERROR\u001b[0m")
