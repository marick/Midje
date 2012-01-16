(ns midje.internal-ideas.t-fact-context
  (:use [midje.internal-ideas.fact-context]
        clojure.test
        midje.test-util))

(def ^:private some-atom (atom nil))

;"creates nested doc-strings from each surrounding context"   

(deftest within-the-nested-contexts-the-doc-strings-build-up
  (within-fact-context "level 1"
    (within-fact-context "level 2"
      (within-fact-context "level 3"
        (reset! some-atom (nested-fact-description)))))
    (is (= @some-atom "level 1 - level 2 - level 3")))
  
(reset! some-atom nil)

(deftest non-existant-context-descriptions-are-ignored-when-printed
    (within-fact-context "level 1"
      (within-fact-context nil
        (within-fact-context "level 3"
          (reset! some-atom (nested-fact-description)))))
      (is (= @some-atom "level 1 - level 3")))

(deftest non-existant-context-descriptions-are-ignored-when-printed
  (within-fact-context "level 1"
    (try
      (within-fact-context "level 2"
        (throw (Exception. "boom")))
      (catch Exception e nil))

    (reset! some-atom (nested-fact-description)))

  (is (= @some-atom "level 1")))

(reset! some-atom nil)

(deftest outside-of-the-contexts-there-is-no-fact-description-at-all 
    (is (= (nested-fact-description) nil)))

(reset! some-atom nil)

(deftest nil-descriptions-produces-blank-nested-fact-description
  (within-fact-context nil 
    (reset! some-atom (nested-fact-description)))
  (is (= @some-atom nil)))