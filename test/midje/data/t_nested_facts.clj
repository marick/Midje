(ns midje.data.t-nested-facts
  (:require [midje.data.nested-facts :refer :all]
            [clojure.test :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.util.pile :as pile]))


(defn faux-fact
  ([] (gensym))
  ([description]
     (with-meta (gensym) {:midje/description description})))

(deftest within-the-nested-contexts-the-doc-strings-build-up
  (in-new-fact (faux-fact "level 1")
    (in-new-fact (faux-fact "level 2")
      (in-new-fact (faux-fact "level 3")
        (is (= (descriptions) ["level 1" "level 2" "level 3"]))))))

(deftest context-descriptions-can-be-nil
    (in-new-fact (faux-fact "level 1")
      (in-new-fact (faux-fact)
        (in-new-fact (faux-fact "level 3")
          (is (= (descriptions) ["level 1" nil "level 3"]))))))

(deftest exceptions-dont-disturb-the-description-nesting
  (in-new-fact (faux-fact "level 1")
    (try
      (in-new-fact (faux-fact "level 2")
        (throw (Exception. "boom")))
      (catch Exception e nil))
    (is (= (descriptions) ["level 1"]))))

(deftest outside-of-the-contexts-there-is-no-fact-description-at-all
    (is (= (descriptions) [])))

(deftest nested-descriptions-can-take-an-argument-to-tack-on
  (in-new-fact (faux-fact "level 1")
    (in-new-fact (faux-fact "level 2")
      (in-new-fact (faux-fact "level 3")
        (is (= (descriptions "level 4")
               ["level 1" "level 2" "level 3" "level 4"]))))))

(fact names-can-be-used-for-descriptions
  (fact
    (fact "too"
      (fact with-names "having lower precedence"
        (descriptions) => ["names-can-be-used-for-descriptions" nil "too" "having lower precedence"]))))



;;; Binding maps
(defn minimal-fact [binding-map-keys binding-map-vals]
  (with-meta (fn[])
    {:midje/table-bindings (pile/ordered-zipmap binding-map-keys binding-map-vals)}))


(fact "multiple binding maps are merged into one"
  (in-new-fact (minimal-fact [:a :b] [1 2])
    (in-new-fact (minimal-fact [:c] [3])
      ;; It happens that binding maps are duplicated in nested
      ;; tables.
      (in-new-fact (minimal-fact [:c] [3])
        (table-bindings) => (:midje/table-bindings (meta (minimal-fact [:a :b :c] [1 2 3])))))))
