(ns ^{:doc "Lexicon of all Midje arrows. Arrows are used in both the sweet and expanded forms."}
  midje.parsing.arrow-symbols)

(def ^{:doc "As a checking arrow: the left side should result in the right side.
  As a prerequisite arrow: the fake should return the thing on the right."}
  => "=>")

(def ^{:doc "Checking arrow. The left side should NOT result in the right side."}
  =not=> "=not=>")

(def ^{:doc "Checking arrow. The left side should NOT result in the right side."}
  =deny=> "=deny=>")

(def ^{:doc "Checking arrow. The left side should macroexpand-1 to the right side."}
  =expands-to=> "=expands-to=>")

(def ^{:doc "Checking arrow. The check won't evaluate, but will
  leave a 'WORK TO DO' reminder in the report."}
  =future=> "=future=>")

(def ^{:doc "Data prerequisite arrow.
  Ex. (fact
        (:a ..m..) => 1
        (provided ..m.. =contains=> {:a 1}))"}
  =contains=> "=contains=>")

(def ^{:doc "Prerequisite arrow. The fake will return the first element
  of the given sequence the first time the fake is evaluated, the second
  element the second time, and so on. Note, that each element is only
  evaluated when needed, thus in the below example the exception is
  never thrown.

  Ex. (def tally [] (+ (bar) (bar) (bar)))
      (fact (tally) => 6
        (provided (bar) =streams=> [1 2 3 (throw (Exception.))]))"}
  =streams=> "=streams=>")

(def ^{:doc "Prerequisite arrow. The fake will throw the Throwable on
  the right side of the arrow.

  Ex. (def f [] (g))
      (fact (f) => (throws IllegalArgumentException. \"boom\")
        (provided (g) =throws=> (IllegalArgumentException. \"boom\")))"}
  =throws=> "=throws=>")

(def ^{:doc "Use this when testing Midje code that processes arrow-forms"}
     =test=> "=test=>")

(def ^{:doc "Use this to generate an exception while parsing"}
     =throw-parse-exception=> "=throw-parse-exception=>")
