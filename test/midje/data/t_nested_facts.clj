(ns midje.data.t-nested-facts
  (:use [midje.data.nested-facts]
        clojure.test
        midje.sweet
        midje.test-util))

;"creates nested doc-strings from each surrounding context"   

(deftest within-the-nested-contexts-the-doc-strings-build-up
  (adds "level 1"
    (adds "level 2"
      (adds "level 3"
        (is (= (descriptions) ["level 1" "level 2" "level 3"]))))))

(deftest context-descriptions-can-be-nil
    (adds "level 1"
      (adds nil
        (adds "level 3"
          (is (= (descriptions) ["level 1" nil "level 3"]))))))

(deftest exceptions-dont-disturbed-the-description-nesting
  (adds "level 1"
    (try
      (adds "level 2"
        (throw (Exception. "boom")))
      (catch Exception e nil))
    (is (= (descriptions) ["level 1"]))))

(deftest outside-of-the-contexts-there-is-no-fact-description-at-all 
    (is (= (descriptions) [])))

(deftest nested-descriptions-can-take-an-argument-to-tack-on
  (adds "level 1"
    (adds "level 2"
      (adds "level 3"
        (is (= (descriptions "level 4")
               ["level 1" "level 2" "level 3" "level 4"]))))))
