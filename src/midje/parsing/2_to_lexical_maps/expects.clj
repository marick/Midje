(ns ^{:doc "generate a map for a particular example"}
  midje.parsing.2-to-lexical-maps.expects
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [midje.parsing.arrow-symbols])
  (:require [clojure.zip :as zip]
            [midje.config :as config]
            [midje.util.pile :as pile]
            [midje.util.exceptions :as exceptions]
            [midje.data.metaconstant :as metaconstant]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.2-to-lexical-maps.data-fakes :as parse-data-fakes]
            [midje.emission.api :as emit])
  (:import midje.data.metaconstant.Metaconstant))

(defn- ^{:testable true } a-fake? [x]
  (and (seq? x)
       (semi-sweet-keyword? (first x))))

(defn mkfn:arrow? [& expected]
  (fn [actual] ((set expected) (name actual))))
(def normal-arrows? (mkfn:arrow? => =not=> =deny=>))
(def macroexpansion-arrow? (mkfn:arrow? =expands-to=>))
(def future-arrow? (mkfn:arrow? =future=>))
(def parse-exception-arrow? (mkfn:arrow? =throw-parse-exception=>))

(defn expansion [call-form arrow expected-result fakes overrides]
  (pred-cond arrow
    normal-arrows?
    (let [check (lexical-maps/example call-form arrow expected-result overrides)
          expanded-fakes (map (fn [fake]
                                 ;; TODO: Maybe this wants to be a multimethod,
                                 ;; but I'm not sure whether the resemblance between
                                 ;; data-fakes and regular fakes isn't too coincidental. 
                                 (cond (first-named? fake "fake")
                                       (parse-fakes/to-lexical-map-form fake)

                                       (first-named? fake "data-fake")
                                       (parse-data-fakes/to-lexical-map-form fake)

                                       (first-named? fake "not-called")
                                       (macroexpand fake)

                                       :else
                                       (do 
                                         (prn "Now here's a peculiar thing to find inside a check: " fake)
                                         fake)))
                               fakes)]
      `(midje.checking.examples/check-one ~check ~(vec expanded-fakes)))
             
    macroexpansion-arrow?
    (let [expanded-macro `(macroexpand-1 '~call-form)
          escaped-expected-result `(quote ~expected-result)]
      (expansion expanded-macro => escaped-expected-result fakes
                 (concat overrides [:expected-result-form escaped-expected-result])))

    future-arrow?
    (let [position (:position (apply hash-map-duplicates-ok overrides))]
        `(emit/future-fact (nested-facts/descriptions ~(str "on `" call-form "`")) ~position))

    parse-exception-arrow?
    (throw (java.lang.ClassNotFoundException. "A test asked for an exception"))
    
    :else
    (throw (Error. (str "Program error: Unknown arrow form " arrow)))))

(defn valid-pieces [[_ call-form arrow expected-result & fakes+overrides]]
  (cond (and (sequential? call-form)
             (= (first call-form) 'provided))
        (error/report-error call-form
                            (cl-format nil "... ~S ~A ~S" call-form arrow expected-result)
                            "It looks as though you've misparenthesized a prerequisite."))
  (let [[fakes overrides] (separate a-fake? fakes+overrides)]
    [call-form arrow expected-result fakes overrides]))

(defn to-lexical-map-form [full-form]
  (apply expansion (valid-pieces full-form)))

