(ns midje.error-handling.t-semi-sweet-validations
  (:use [midje.sweet]
        [midje.error-handling validation-errors semi-sweet-validations]
        [midje.parsing.util.file-position :only [form-position]]
        [midje.test-util]))

(facts "expect validation returns tail part of structure"
  (let [correct '(expect (f 1) => 3)]
    (validate correct) =not=> validation-error-form?
    (validate correct) => '[(f 1) => 3]))

(facts "data fake validation returns whole data-fake form"
  (let [valid-data-fake '(data-fake ..mc.. =contains=> {:foo 'bar})]
    (validate valid-data-fake) =not=> validation-error-form?
    (validate valid-data-fake) => valid-data-fake
    (validate (list valid-data-fake valid-data-fake valid-data-fake)) 
           => (list valid-data-fake valid-data-fake valid-data-fake)))


;;; Full-bore tests.

(silent-fake a => 3)
(note-that parser-exploded,
           (fact-failed-with-note #"must look like a function call")
           (fact-failed-with-note #"`a` doesn't"))

(silent-expect (throw "should not be evaluated") => 3 (fake a => 3))
(note-that parser-exploded,
           (fact-failed-with-note #"must look like a function call")
           (fact-failed-with-note #"`a` doesn't"))

(silent-data-fake (f 1) =contains=> {:a 1})
(note-that parser-exploded,
           (fact-failed-with-note #"no metaconstant"))

(future-fact "handle case where =contains=> is left out"
  (+ 1 1) => 2
  (provided ..m.. => 3))

