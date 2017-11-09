(ns midje.emission.plugins.t-default-end-to-end
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.config :as config]))

(capturing-failure-output
 (fact (+ 1 1) => 3)
 (fact
   @fact-output => #"FAIL"
   (strip-ansi-coloring @fact-output) => #"Expected:\s+3"
   (strip-ansi-coloring @fact-output) => #"Actual:\s+2"))

(capturing-failure-output
 (fact (+ 1 1) =not=> 2)
 (fact
   @fact-output => #"FAIL"
   (strip-ansi-coloring @fact-output) => #"Expected: Anything BUT"
   (strip-ansi-coloring @fact-output) => #"2"
   (strip-ansi-coloring @fact-output) => #"Actual:"
   (strip-ansi-coloring @fact-output) => #"2"))

(capturing-failure-output
 (fact (+ 1 1) => odd?)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s+2"
   @fact-output => #"Checking function:\s+odd\?"))

(capturing-failure-output
 (fact (+ 1 1) =not=> even?)
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"NOT supposed to agree.*checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s+2"
   @fact-output => #"Checking function:\s+even\?"))

(capturing-fact-output
 (config/with-augmented-config {:visible-future true}
   (future-fact 1 => 2))
 (fact @fact-output => #"(?s)WORK TO DO\S* at \(t_default_end_to_end"))

(capturing-fact-output
 (config/with-augmented-config {:visible-future true}
   (future-fact :some-metadata "fact name" 1 => 2))
 (fact @fact-output => #"(?s)WORK TO DO\S* \"fact name\" at \(t_default_end_to_end"))

(capturing-fact-output
 (config/with-augmented-config {:visible-future true}
   (fact "outer"
     (fact "inner" (cons (first 3)) =future=> 2)))
 (fact @fact-output => #"(?s)WORK TO DO\S* \"outer - inner - on `\(cons \(first 3\)\)`\" at \(t_default_end_to_end"))

(capturing-failure-output
 (fact (cons 1 nil) => (contains 2))
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s+\(1\)"
   @fact-output => #"Checking function:\s+\(contains 2\)"
   @fact-output => #"Best match found:\s+\[\]"))

(capturing-failure-output
 (fact (cons 1 nil) =not=> (just 1))
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"NOT supposed to agree.*checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s\(1\)"
   @fact-output => #"Checking function:\s+\(just 1\)"))

(capturing-failure-output
 (fact 5 => (chatty-checker [a] (and (= a 5) (= a 6))))
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s+5"
   @fact-output => #"Checking function:\s+\(chatty-checker \[a\] \(and \(= a 5\) \(= a 6\)\)\)"
   (strip-ansi-coloring @fact-output) => #"\(= a 5\) => true"
   (strip-ansi-coloring @fact-output) => #"\(= a 6\) => false"))

(capturing-failure-output
 (fact 5 => (every-checker even? (throws "message")))
 (fact
   @fact-output => #"FAIL"
   @fact-output => #"checking function"
   (strip-ansi-coloring @fact-output) => #"Actual result:\s+5"
   @fact-output => #"Checking function:\s+\(every-checker even\? \(throws \"message\"\)\)"
   (strip-ansi-coloring @fact-output) => #"even\? => false"))
