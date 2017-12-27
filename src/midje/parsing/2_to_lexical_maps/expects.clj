(ns midje.parsing.2-to-lexical-maps.expects
  "generate a map for a particular checkable"
  (:require [midje.data.nested-facts :as nested-facts]
            [midje.emission.api :as emit]
            [midje.parsing.2-to-lexical-maps.data-fakes :as parse-data-fakes]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.recognizing :as recognize]
            [clojure.pprint :as pprint]
            [such.control-flow :refer [branch-on]]
            [such.maps :as map]
            [such.sequences :as seq]))


;; TODO: Maybe this wants to be a multimethod, but I'm not sure whether the
;; resemblance between data-fakes and regular fakes isn't too coincidental.
(defn- expand-fake [fake]
 (cond (first-named? fake "fake")
       (parse-fakes/to-lexical-map-form fake)

       (first-named? fake "data-fake")
       (parse-data-fakes/to-lexical-map-form fake)

       (first-named? fake "not-called")
       (macroexpand fake)

       :else
       (do (prn "Now here's a peculiar thing to find inside a check: " fake)
           fake)))

(defn expansion [call-form arrow expected-result fakes overrides]
  (branch-on arrow
    recognize/common-check-arrow?
    (let [check (lexical-maps/checkable call-form arrow expected-result overrides)
          expanded-fakes (map expand-fake fakes)]
      `(midje.checking.checkables/check-one ~check ~(vec expanded-fakes)))

    recognize/macroexpansion-check-arrow?
    (let [expanded-macro `(macroexpand-1 '~call-form)
          escaped-expected-result `(quote ~expected-result)]
      (expansion expanded-macro => escaped-expected-result fakes
                 (concat overrides [:expected-result-form escaped-expected-result])))

    recognize/future-check-arrow?
    (let [position (:position (apply hash-map overrides))]
        `(emit/future-fact (nested-facts/descriptions ~(str "on `" call-form "`")) ~position))

    recognize/parse-exception-arrow?
    (throw (java.lang.ClassNotFoundException. "A test asked for an exception"))

    :else
    (throw (Error. (str "Program error: Unknown arrow form " arrow)))))

(defn- valid-pieces [[_ call-form arrow expected-result & fakes+overrides]]
  (cond (and (sequential? call-form)
             (= (first call-form) 'provided))
        (error/report-error call-form
                            (pprint/cl-format nil "... ~S ~A ~S" call-form arrow expected-result)
                            "It looks as though you've misparenthesized a prerequisite."))
  (let [[fakes overrides] (seq/bifurcate recognize/fake? fakes+overrides)]
    [call-form arrow expected-result fakes overrides]))

(defn to-lexical-map-form [full-form]
  (apply expansion (valid-pieces full-form)))

(defmacro expect
  {:arglists '([call-form arrow expected-result & fakes+overrides])}
  [& _]
  (to-lexical-map-form &form))

