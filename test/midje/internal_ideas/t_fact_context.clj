(ns midje.internal-ideas.t-fact-context
  (:use [midje.internal-ideas.fact-context]
        clojure.test
        midje.sweet
        midje.test-util))

;"creates nested doc-strings from each surrounding context"   

(deftest within-the-nested-contexts-the-doc-strings-build-up
  (within-fact-context "level 1"
    (within-fact-context "level 2"
      (within-fact-context "level 3"
        (is (= @nested-descriptions ["level 1" "level 2" "level 3"]))))))

(deftest context-descriptions-can-be-nil
    (within-fact-context "level 1"
      (within-fact-context nil
        (within-fact-context "level 3"
          (is (= @nested-descriptions ["level 1" nil "level 3"]))))))

(deftest exceptions-dont-disturbed-the-description-nesting
  (within-fact-context "level 1"
    (try
      (within-fact-context "level 2"
        (throw (Exception. "boom")))
      (catch Exception e nil))
    (is (= @nested-descriptions ["level 1"]))))

(deftest outside-of-the-contexts-there-is-no-fact-description-at-all 
    (is (= @nested-descriptions [])))

(tabular "nested-descriptions can be formatted as '-' separated"
  (fact 
    (format-nested-descriptions descriptions) => result)
  
  descriptions      result
  ["a" "b" "c"]     "a - b - c" 
  ["a" nil "c"]     "a - c"
  nil               nil
  []                nil )
