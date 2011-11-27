;; -*- indent-tabs-mode: nil -*-

(ns midje.unprocessed
  (:use clojure.test
        [midje.internal-ideas.fakes]
        [midje.ideas.background :only [background-fakes]]
        [midje.util laziness report]
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.chatty :only [chatty-checker?]]
        [midje.checkers.util]
        [midje.util.old-clojure-contrib.ns-utils :only [immigrate]]
        [clojure.tools.macro :only [macrolet]]))
(immigrate 'midje.checkers)


(defmulti ^{:private true} check-result (fn [actual call]
                         (:desired-check call)))

(defmethod check-result :check-match [actual call]
  (cond (extended-= actual (:expected-result call))
        (report {:type :pass})

        (fn? (call :expected-result))
        (report (merge {:type :mock-expected-result-functional-failure
                        :binding-note (:binding-note call)
                        :position (:position call)
                        :expected (:expected-result-text-for-failures call) }
                       (if (chatty-checker? (:expected-result call))
                         (do
                           (let [chatty-result ((:expected-result call) actual)]
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
                 :position (:position call)
                 :binding-note (:binding-note call)
                 :actual actual
                 :expected (:expected-result call) })))

(defmethod check-result :check-negated-match [actual call]
   (cond (not (extended-= actual (:expected-result call)))
         (report {:type :pass})

        (fn? (call :expected-result))
        (report {:type :mock-actual-inappropriately-matches-checker
                 :binding-note (:binding-note call)
                 :position (:position call)
                 :expected (:expected-result-text-for-failures call)
                 :actual actual})

        :else
        (report {:type :mock-expected-result-inappropriately-matched
                 :binding-note (:binding-note call)
                 :position (:position call)
                 :expected (:expected-result-text-for-failures call) 
                 :actual actual})))

(defn expect* [unprocessed-check local-fakes]
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  (macrolet [(capturing-exception [form]
               `(try ~form
                  (catch Throwable e#
                    (captured-exception e#))))]
    (with-installed-fakes (concat (reverse (filter :data-fake (background-fakes))) local-fakes)
      (let [code-under-test-result (capturing-exception
                                    (eagerly
                                     ((:function-under-test unprocessed-check))))]
        (check-call-counts local-fakes)
        (check-result code-under-test-result unprocessed-check)
        :irrelevant-return-value))))
