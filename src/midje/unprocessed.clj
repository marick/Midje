;; -*- indent-tabs-mode: nil -*-

(ns midje.unprocessed
  (:use clojure.test
        [midje.fakes]
        [midje.util laziness report thread-safe-var-nesting]
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.chatty :only [chatty-checker?]]
        [midje.checkers.util]
        [clojure.contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.checkers)


(defn- background-fakes-plus [fakes]
  (concat fakes (namespace-values-inside-out :midje/background-fakes)))

;; TODO: I'm not wild about signalling failure in two ways: by report() and by
;; return value. Fix this when (a) we move away from clojure.test.report and
;; (b) we figure out how to make fact() some meaningful unit of reporting.
;;
;; Later note: this doesn't actually work well anyway when facts are nested within
;; larger structures. Probably fact should return true/false based on interior failure
;; counts.
(defn check-result [actual call]
  (cond (extended-= actual (call :expected-result))
        (do (report {:type :pass})
            true)

        (fn? (call :expected-result))
        (do (report (merge {:type :mock-expected-result-functional-failure
                            :position (call :position)
                            :expected (call :expected-result-text-for-failures) }
                           (if (chatty-checker? (call :expected-result))
                             (do
                               (let [chatty-result ((call :expected-result) actual)]
                                 (if (map? chatty-result)
                                   chatty-result
                                   {:actual actual
                                    :notes ["Midje program error. Please report."
                                            (str "A chatty checker returned "
                                                 (pr-str chatty-result)
                                                 " instead of a map.")]})))
                             {:actual actual})))
            false)
        
        :else
        (do 
          (report {:type :mock-expected-result-failure
                   :position (call :position)
                   :actual actual
                   :expected (call :expected-result) })
          false))
)

(defmacro capturing-exception [form]
  `(try ~form
        (catch Throwable e#
          (captured-exception e#))))

(defn expect* [call-map local-fakes]
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  (let [fakes (background-fakes-plus local-fakes)]
    (with-altered-roots (binding-map fakes)
      (let [code-under-test-result (capturing-exception
                                    (eagerly
                                     ((call-map :function-under-test))))]
        (check-call-counts fakes)
        (check-result code-under-test-result call-map)))))
