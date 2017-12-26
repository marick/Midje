(ns midje.emission.plugins.t-util
  (:require [midje.config :as config]
            [midje.sweet :refer :all]
            [midje.test-util :as test-util]
            [midje.emission.plugins.util :refer :all]))

(fact "line structures can be linearized"
  (linearize-lines [ ["1" [nil] "2" nil] [] "3" [[["4"]]]]) => ["1" "2" "3" "4"])

(defrecord R [x m a])

(defrecord Container [value]
  Object
  (toString [this] (str value)))

(fact "Different forms can be attractively printed"
  (test-util/strip-ansi-coloring
    (attractively-stringified-value even?)) => "\"core/even?\""
  (test-util/strip-ansi-coloring
    (attractively-stringified-value midje.emission.plugins.util/emit-lines)) => "\"util/emit-lines\""
  (attractively-stringified-value (fn [n] 1)) => #"fn--"
  ;; Note ordering
  (test-util/strip-ansi-coloring
    (attractively-stringified-value {:b 2 :a 1})) => "{:a 1 :b 2}"
  (test-util/strip-ansi-coloring
    (attractively-stringified-value #{9 6 2 7 1 3})) => "#{1 2 3 6 7 9}"
  (test-util/strip-ansi-coloring
    (attractively-stringified-value #{[1] [:a]})) => (some-checker "#{[1] [:a]}" "#{[:a] [1]}")
  (test-util/strip-ansi-coloring
    (attractively-stringified-value {[1] "1" [:a] "a"})) => (some-checker "{[1] \"1\" [:a] \"a\"}" "{[:a] \"a\" [1] \"1\"}"))

(facts "Attractively printing records"
  (fact "uses the dispatch macro classname notation"
    (test-util/strip-ansi-coloring
      (attractively-stringified-value (R. 1 2 3))) => #"\#[^\s\#]+\ \{:a 3 :m 2 :x 1\}")
  (fact "It should be visible to users when records are nested within records"
    (test-util/strip-ansi-coloring
      (attractively-stringified-value (Container. (R. 1 2 3)))) => #"\#[^\s\#]+\s\{:value \#[^\s\#]+\s\{:a 3 :m 2 :x 1\}}"))

(facts "Configuring :pretty-print settings"
  (fact "You can turn off fancy pretty printing"
    (config/with-augmented-config {:pretty-print false}
      (attractively-stringified-value {:b 2 :a 1}) => "{:a 1, :b 2}"))
  (fact "Record printing w/o pretty printing. Element ordering isn't guaranteed"
    (config/with-augmented-config {:pretty-print false}
      (attractively-stringified-value (Container. (R. 1 2 3))) => #"\#[^\s\#]+\{:value \#[^\s\#]+\{:[a-z] \d, :[a-z] \d, :[a-z] \d\}}"))
  (fact "By default pretty-printing is on, and will print formatted, colored output"
    (test-util/strip-ansi-coloring
      (attractively-stringified-value {:b 2 :a 1})) => "{:a 1 :b 2}"
    (attractively-stringified-value {:b 2 :a 1}) =not=> "{:a 1 :b 2}"))

(tabular "descriptions harvested from nested facts can be formatted as '-' separated"
  (fact
    (format-nested-descriptions descriptions) => result)

  descriptions      result
  ["a" "b" "c"]     "a - b - c"
  ["a" nil "c"]     "a - c"
  nil               nil
  []                nil
  [nil]             nil)

(fact "string positions have filenames and line numbers"
  (filename-lineno ["filename.clj" 33]) => "(filename.clj:33)"
  (fact "produces something innocuous when the line number is nil"
    (filename-lineno ["filename.clj" nil]) => "(filename.clj:null)"))

(fact "There is a normal way to describe a failure"
  (failure-notice {:position ["filename" 10]})
  => (just #"FAIL\S* at \(filename:10\)" nil)
  (failure-notice {:position ["filename" 10] :description ["outer" nil "inner"]})
  => (just #"FAIL\S* \"outer - inner\" at \(filename:10\)" nil)
  (failure-notice {:position ["filename" 10] :midje/table-bindings '{?a 1}})
  => (just #"FAIL\S* at \(filename:10\)"
           "With table substitutions: [?a 1]")

  (config/with-augmented-config {:visible-failure-namespace true}
    (failure-notice {:position ["filename" 10] :namespace "example.ns.file"})
    => (just #"FAIL\S* at \(filename:10  example.ns.file\)" nil)))
