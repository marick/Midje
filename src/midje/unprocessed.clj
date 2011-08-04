;; -*- indent-tabs-mode: nil -*-

(ns midje.unprocessed
  (:use clojure.test
        [midje.internal-ideas.fakes]
        [midje.util laziness report]
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.chatty :only [chatty-checker?]]
        [midje.checkers.util]
        [clojure.contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.checkers)


;; TODO: I'm not wild about signalling failure in two ways: by report() and by
;; return value. Fix this when (a) we move away from clojure.test.report and
;; (b) we figure out how to make fact() some meaningful unit of reporting.
;;
;; Later note: this doesn't actually work well anyway when facts are nested within
;; larger structures. Probably fact should return true/false based on interior failure
;; counts.
(defmulti check-result (fn [actual call]
                         (:desired-check call)))

(defmethod check-result :check-match [actual call]
  (cond (extended-= actual (call :expected-result))
        (report {:type :pass})

        (fn? (call :expected-result))
        (report (merge {:type :mock-expected-result-functional-failure
                        :binding-note (call :binding-note)
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
        :else
        (report {:type :mock-expected-result-failure
                 :position (call :position)
                 :binding-note (call :binding-note)
                 :actual actual
                 :expected (call :expected-result) })))

(defmethod check-result :check-negated-match [actual call]
   (cond (not (extended-= actual (call :expected-result)))
         (report {:type :pass})

        (fn? (call :expected-result))
        (report {:type :mock-actual-inappropriately-matches-checker
                 :binding-note (call :binding-note)
                 :position (call :position)
                 :expected (call :expected-result-text-for-failures)
                 :actual actual})

        :else
        (report {:type :mock-expected-result-inappropriately-matched
                 :binding-note (call :binding-note)
                 :position (call :position)
                 :expected (call :expected-result-text-for-failures) 
                 :actual actual})))

(defmacro capturing-exception [form]
  `(try ~form
        (catch Throwable e#
          (captured-exception e#))))

(defn expect* [call-map local-fakes]
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  (with-installed-fakes local-fakes
    (let [code-under-test-result (capturing-exception
                                  (eagerly
                                   ((call-map :function-under-test))))]
      (check-call-counts local-fakes)
      (check-result code-under-test-result call-map)
      :irrelevant-return-value)))
