;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling.t_semi_sweet_validations
  (:use [midje.sweet]
        [midje.error-handling validation-errors semi-sweet-validations]
        [midje.internal-ideas.file-position :only [form-position]]
        [midje.test-util]))

(facts "expect validation returns tail part of structure"
  (let [correct '(expect (f 1) => 3)]
    (validate correct) =not=> validation-error-form?
    (validate correct) => '[(f 1) => 3]))

(facts "fake validation returns whole fake form"
  (let [valid-fake '(fake (f 1) => 3)]
    (validate valid-fake) =not=> validation-error-form?
    (validate valid-fake) => valid-fake
    (validate (list valid-fake valid-fake valid-fake)) 
           => (list valid-fake valid-fake valid-fake)))

(facts "data fake validation returns whole data-fake form"
  (let [valid-data-fake '(data-fake ..mc.. =contains=> {:foo 'bar})]
    (validate valid-data-fake) =not=> validation-error-form?
    (validate valid-data-fake) => valid-data-fake
    (validate (list valid-data-fake valid-data-fake valid-data-fake)) 
           => (list valid-data-fake valid-data-fake valid-data-fake)))

; Duplication of validate is because of bug in against-background.
(facts "errors are so tagged and contain file position"
  (against-background (form-position anything) => ...position...)

  (let [too-short '(expect (f 1) =>)]
    (validate too-short) => validation-error-form?
    (str (validate too-short)) => (contains "...position..."))
  
  (let [bad-left-side '(fake a => 3)]
    (validate bad-left-side) => validation-error-form?
    (str (validate bad-left-side)) => (contains "...position...")))

;;; Full-bore tests.

(after-silently
 (expect (+ 1 2) =>)
 (expect @reported => (validation-error-with-notes
                        (contains "(expect (+ 1 2) =>")
                        (contains "(expect <actual> => <expected>"))))

(after-silently
 (fake a => 3)
 (expect @reported => (validation-error-with-notes
                        #"must look like a function call"
                        #"`a` doesn't")))

(after-silently
 (expect (throw "should not be evaluated") => 3 (fake a => 3))
 (expect @reported => (validation-error-with-notes
                        #"must look like a function call"
                        #"`a` doesn't")))

(after-silently
 (data-fake (f 1) =contains=> {:a 1})
 (expect @reported => (validation-error-with-notes #"no metaconstant")))

(future-fact "handle case where =contains=> is left out"
  (+ 1 1) => 2
  (provided ..m.. => 3))

