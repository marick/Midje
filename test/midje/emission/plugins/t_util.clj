(ns midje.emission.plugins.t-util
  (:use midje.sweet
        midje.emission.plugins.util))

(fact "line structures can be linearized"
  (linearize-lines [ ["1" [nil] "2" nil] [] "3" [[["4"]]]]) => ["1" "2" "3" "4"])


(fact "Different forms can be attractively printed"
  (attractively-stringified-form even?) => "core/even?"
  (attractively-stringified-form midje.emission.plugins.util/emit-lines) => "util/emit-lines"
  (attractively-stringified-form (fn [n] 1)) => #"fn--"
  (attractively-stringified-form {:b 2 :a 1}) => "{:a 1, :b 2}"
  (attractively-stringified-form #{9 6 2 7 1 3}) => "#{1 2 3 6 7 9}")


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
  (failure-notice {:position ["filename" 10] :binding-note "a big string"})
  => (just #"FAIL\S* at \(filename:10\)"
           "With table substitutions: a big string"))
