
;; This file is used to check whether facts can be compiled out.

(fact
  (throw (Exception. "This is supposed to compile out")))
