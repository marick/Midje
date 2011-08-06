;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t-semi-sweet-errors
  (:use [midje.sweet]
        [midje.error-handling monadic semi-sweet-errors]
        [midje.internal-ideas.file-position :only [form-position]]
        [midje.test-util]))

(facts "expect validation returns tail part of structure"
  (let [correct '(expect (f 1) => 3)]
    (validate correct) =not=> user-error-form?
    (validate correct) => '[(f 1) => 3]))

; Duplication of validate is because of bug in against-background.
(facts "errors are so tagged and contain file position"
  (against-background (form-position anything) => ...position...)

  (let [too-short '(expect (f 1) =>)]
    (validate too-short) => user-error-form?
    (str (validate too-short)) => (contains "...position..."))
  
  (let [bad-left-side '(fake a => 3)]
    (validate bad-left-side) => user-error-form?
    (str (validate bad-left-side)) => (contains "...position...")))

;;; Full-bore tests.

(defmacro user-error-with-notes [& notes]
  `(just (contains {:notes (just ~@notes)
                    :type :user-error})))
  

(after-silently
 (expect (+ 1 2) =>)
 (expect @reported => (user-error-with-notes
                        (contains "(expect (+ 1 2) =>")
                        (contains "(expect <actual> => <expected>"))))

(after-silently
 (fake a => 3)
 (expect @reported => (user-error-with-notes
                        #"must look like a function call"
                        #"`a` doesn't")))

(after-silently
 (expect (throw "should not be evaluated") => 3 (fake a => 3))
 (expect @reported => (user-error-with-notes
                        #"must look like a function call"
                        #"`a` doesn't")))

(after-silently
 (data-fake (f 1) =contains=> {:a 1})
 (expect @reported => (user-error-with-notes #"no metaconstant")))

(future-fact "handle case where =contains=> is left out"
  (+ 1 1) => 2
  (provided ..m.. => 3))

