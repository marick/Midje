(ns midje.emission.plugins.t-default-failure-lines
  (:require [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.emission.plugins.default-failure-lines :refer :all]
            [midje.emission.plugins.util :as util]))


;;; Note that this will scrozzle the failure notice from the facts below
;;; if they fail.
(against-background [(util/failure-notice anything) => "notice"]

  (fact "the simplest kind of failure"
    (fact "is a mismatch, which shows the calculated expected result (not the original form)"
      (map strip-ansi-coloring
           (summarize {:type :actual-result-did-not-match-expected-value
                       :expected-result 1, :actual 2}))
      => (just "notice" #"Expected:\n1", #"Actual:\n2")

      ;; in general...
      (map strip-ansi-coloring
           (summarize {:type :actual-result-did-not-match-expected-value
                       :expected-result ..expected.. :actual ..actual..}))
      => (just "notice" #"Expected:\nEEE", #"Actual:\nAAA")
      (provided
        (util/attractively-stringified-value ..expected..) => 'EEE
        (util/attractively-stringified-value ..actual..) => 'AAA))

    (fact "or an unexpected match"
      (map strip-ansi-coloring
           (summarize {:type :actual-result-should-not-have-matched-expected-value
                       :expected-result 2, :actual 2}))
      => (just "notice" #"Expected: Anything BUT\n2", #"Actual:\n2")

      ;; in general...
      (map strip-ansi-coloring
           (summarize {:type :actual-result-should-not-have-matched-expected-value
                       :expected-result ..expected.. :actual ..actual..}))
      => (just "notice" #"Expected: Anything BUT\nEEE", #"Actual:\nAAA")
      (provided
        (util/attractively-stringified-value ..expected..) => 'EEE
        (util/attractively-stringified-value ..actual..) => 'AAA))

    (fact "will sort both actual and expected results when appropriate"
      (let [sequences-inappropriate '["not" "with" "sequence"]
            but-sets-are #{:a :but :set :sorted :with}]
        (map strip-ansi-coloring
             (summarize {:type :actual-result-should-not-have-matched-expected-value,
                         :actual sequences-inappropriate :expected-result sequences-inappropriate}))
        => (contains [#"\[\"not\" \"with\" \"sequence\"\]"
                      #"\[\"not\" \"with\" \"sequence\"\]"] :gaps-ok)

        (map strip-ansi-coloring
             (summarize {:type :actual-result-did-not-match-expected-value
                         :actual but-sets-are :expected-result but-sets-are}))
        => (contains [#"#\{:a :but :set :sorted :with\}"
                      #"#\{:a :but :set :sorted :with\}"]
                     :gaps-ok))))

  (fact "checkers"
    (map strip-ansi-coloring
         (summarize {:type :actual-result-did-not-match-checker
                     :actual 2, :expected-result-form 'odd?}))
    => (just "notice" "Actual result did not agree with the checking function."
             #"Actual result:\n2" #"Checking function: odd")

    (fact "can supply intermediate results"
    (map strip-ansi-coloring
         (summarize {:type :actual-result-did-not-match-checker
                     :actual 2, :expected-result-form 'checker
                     :intermediate-results '[[(f 1) "3"] [(f 2) 3]]}))
      => (just "notice" "Actual result did not agree with the checking function."
               #"Actual result:\n2"
               #"Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 1) => \"3\"")
               (contains "(f 2) => 3")))

    (fact "can also supply notes"
      (map strip-ansi-coloring
           (summarize {:type :actual-result-did-not-match-checker
                       :actual 2, :expected-result-form 'checker
                       :notes ["note 1" "note 2"]}))
      => (just "notice" "Actual result did not agree with the checking function."
               #"Actual result:\n2"
               #"Checking function: checker"
               #"The checker said this about the reason:"
               #"note 1"
               #"note 2"))

    (fact "prettify the actual value and intermediate results"
      (map strip-ansi-coloring
           (summarize {:type :actual-result-did-not-match-checker
                       :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                       :expected-result-form 'checker
                       :intermediate-results '[[(f 2) #{15 3 7 2}]]}))
      => (just "notice" "Actual result did not agree with the checking function."
               #"Actual result:\n\{:a 5 :d 7 :f 6 :p 3 :r 4 :z 2\}"
               #"Checking function: checker"
               #"During checking, these intermediate values were seen:"
               (contains "(f 2) => #{2 3 7 15}")))

    (fact "can also be incorrectly matched"
      (map strip-ansi-coloring
           (summarize {:type :actual-result-should-not-have-matched-checker
                       :actual {:z 2 :p 3 :r 4 :a 5 :f 6 :d 7}
                       :expected-result-form '(collection 3)}))
      => (just "notice" "Actual result was NOT supposed to agree with the checking function."
               #"Actual result:\n\{:a 5 :d 7 :f 6 :p 3 :r 4 :z 2\}"
               #"Checking function: \(collection 3\)")))

  (fact "prerequisites"
    (fact "called with unexpected arguments"
      (fact "a typical case"
        (summarize {:type :prerequisite-was-called-with-unexpected-arguments
                    :actual '(nil)
                    :var #'odd?})
        => (just "notice"
                 #"never said .*#'odd.* would be called"
                 #"\[nil\]"))
      (fact "somewhat more complicated arguments"
        (summarize {:type :prerequisite-was-called-with-unexpected-arguments
                    :actual (list #'cons [1 2 3] "foo")
                    :var #'odd?})
        => (contains #"\[#'clojure.core/cons \[1 2 3\] \"foo\"\]")))
    (fact "incorrect call count"
      (fact "the never-called case"
        (summarize {:type :some-prerequisites-were-called-the-wrong-number-of-times
                    :failures [{:actual-count 0
                                :expected-count :default
                                :expected-result-form "(f a)"}] })
        => (just "notice"
                 #"These calls were not made the right number of times"
                 #"\(f a\).*expected at least once"))
      (fact "the case with a specific number of expected calls"
        (summarize {:type :some-prerequisites-were-called-the-wrong-number-of-times
                    :failures [{:actual-count 3
                                :expected-count [1 2]
                                :expected-result-form "(f a)" }] })
        =>  (just "notice"
                  #"These calls were not made the right number of times"
                  #"\(f a\).*expected :times \[1 2\].*actually called three times"))))

  (fact "errors found during parsing"
    (summarize {:type :parse-error
                :notes ["message"]})
    => (just "notice"
             #"Midje could not understand something you wrote"
             #"message"))


  (facts "about reporting user errors detected because of an exception"
    (summarize {:type :exception-during-parsing
                :macro-form '(foo bar)
                :stacktrace ["one" "two"]})
    => (contains ["notice"
                  #"Midje caught an exception"
                  #"\(foo bar\)"
                  #"stack trace"
                  #"one"
                  #"two"]
                 :gaps-ok))

)  ;; Against-background
