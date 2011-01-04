;; -*- indent-tabs-mode: nil -*-

(ns midje.unprocessed
  (:use clojure.test
        [midje.fakes]
        [midje.util laziness report thread-safe-var-nesting]
        clojure.contrib.error-kit
        [clojure.contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.util.checkers)


(defn- background-fakes-plus [fakes]
  (concat fakes (namespace-values-inside-out :midje/background-fakes)))

(defn expect* [call-map local-fakes]
  "The core function in unprocessed Midje. Takes a map describing a call and a 
   list of maps, each of which describes a secondary call the first call is supposed to 
   make. See the documentation at http://github.com/marick/Midje."
  (let [fakes (background-fakes-plus local-fakes)]
    (with-altered-roots (binding-map fakes)
      (let [code-under-test-result (capturing-exception
                                    (eagerly
                                     ((call-map :function-under-test))))]
        (check-call-counts fakes)
        (check-result code-under-test-result call-map)))))
