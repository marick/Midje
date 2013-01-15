(ns midje.emission.plugins.t-default-failure-lines
  (:use [midje sweet util test-util]
        midje.emission.plugins.default-failure-lines)
  (:require [midje.emission.plugins.util :as util]))


(against-background [(util/failure-notice anything) => "notice"]

  (fact "the simplest kind of failure"
    (fact "is a mismatch"
      (summarize {:type :mock-expected-result-failure :expected-result-form 1, :actual 2})
      => (just "notice" #"\s+Expected: 1", #"\s+Actual: 2")

      ;; in general...
      (summarize {:type :mock-expected-result-failure :expected-result-form 'expected :actual ..actual..})
      => (just "notice" #"\s+Expected: expected", #"\s+Actual: AAA")
      (provided
        (util/attractively-stringified-value ..actual..) => 'AAA))
    
    (fact "or an unexpected match" 
      (summarize {:type :mock-expected-result-inappropriately-matched :expected-result-form 2, :actual 2})
      => (just "notice" #"\s+Expected: Anything BUT 2", #"\s+Actual: 2")
      
      ;; in general...
      (summarize {:type :mock-expected-result-inappropriately-matched :expected-result-form 'expected :actual ..actual..})
      => (just "notice" #"\s+Expected: Anything BUT expected", #"\s+Actual: AAA")
      (provided
        (util/attractively-stringified-value ..actual..) => 'AAA))

    (fact "can sort the expected result if appropriate"
      (summarize {:type :mock-expected-result-failure, :actual 2
                  :expected-result-form '["not" "with" "sequence"]})
      => (contains #"\[\"not\" \"with\" \"sequence\"\]")

      (summarize {:type :mock-expected-result-failure :actual 2
                  :expected-result-form '#{:but :sorted :with :a :set}})
      => (contains #"#\{:a :but :set :sorted :with\}")))

  (fact "checkers"
    (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-result-form 'odd?})
    => (just "notice" "Actual result did not agree with the checking function."
             #"\s+Actual result: 2" #"\s+Checking function: odd")

    (fact "can supply intermediate results"
      (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-result-form 'checker
                  :intermediate-results '[[(f 1) "3"] [(f 2) 3]]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: 2"
               #"\s+Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 1) => \"3\"")
               (contains "(f 2) => 3")))

    (fact "can also supply notes"
      (summarize {:type :mock-expected-result-functional-failure :actual 2, :expected-result-form 'checker
                  :notes ["note 1" "note 2"]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: 2"
               #"\s+Checking function: checker"
               #"The checker said this about the reason:"
               #"note 1"
               #"note 2"))

    (fact "prettify the actual value and intermediate results"
      (summarize {:type :mock-expected-result-functional-failure
                  :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                  :expected-result-form 'checker
                  :intermediate-results '[[(f 2) #{15 3 7 2}]]})
      => (just "notice" "Actual result did not agree with the checking function."
               #"\s+Actual result: \{:a 5, :d 7, :f 6, :p 3, :r 4, :z 2\}"
               #"\s+Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 2) => #{2 3 7 15}")))

    (fact "can also be incorrectly matched"
      (summarize {:type :mock-actual-inappropriately-matches-checker
                  :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                  :expected-result-form '(collection 3)})
      => (just "notice" "Actual result was NOT supposed to agree with the checking function."
               #"\s+Actual result: \{:a 5, :d 7, :f 6, :p 3, :r 4, :z 2\}"
               #"\s+Checking function: \(collection 3\)"))
    )


  (fact "prerequisites" 
    (fact "called with unexpected arguments"
      (fact "a typical case"
        (summarize {:type :mock-argument-match-failure
                    :actual '(nil)
                    :var #'odd?})
        => (just "notice"
                 #"never said .*#'odd.* would be called"
                 #"\(nil\)"))
      (fact "somewhat more complicated arguments"
        (summarize {:type :mock-argument-match-failure
                    :actual (list #'cons [1 2 3] "foo")
                    :var #'odd?})
        => (contains #"\(#'clojure.core/cons \[1 2 3\] \"foo\"\)")))
    (fact "incorrect call count"
      (fact "the never-called case"
        (summarize {:type :mock-incorrect-call-count
                    :failures [{:actual-count 0
                                :expected-count nil
                                :expected-result-form "(f a)"}] })
        => (just "notice"
                 #"These calls were not made the right number of times"
                 #"\(f a\).*expected at least once"))
      (fact "the case with a specific number of expected calls"
        (summarize {:type :mock-incorrect-call-count
                    :failures [{:actual-count 3
                                :expected-count [1 2]
                                :expected-result-form "(f a)" }] })
        =>  (just "notice"
                  #"These calls were not made the right number of times"
                  #"\(f a\).*expected :times \[1 2\].*actually called three times"))))

  (fact "errors found during parsing"
    (summarize {:type :validation-error
                :notes ["message"]})
    => (just "notice"
             #"Midje could not understand something you wrote"
             #"message"))


  (facts "about reporting user errors detected because of an exception"
    (summarize {:type :exceptional-user-error
                :macro-form '(foo bar)
                :stacktrace ["one" "two"]})
    => (just "notice"
             #"Midje caught an exception"
             #"\(foo bar\)"
             #"stack trace"
             #"one"
             #"two"))
  
)  ;; Against-background
