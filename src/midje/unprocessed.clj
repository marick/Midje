;; -*- indent-tabs-mode: nil -*-

(ns midje.unprocessed
  (:use clojure.test
        [midje.internal-ideas.fakes]
        [midje.ideas.background :only [background-fakes]]
        [midje.util laziness report]
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.chatty :only [chatty-checker?]]
        [midje.checkers.util]
        [midje.util.namespace :only [immigrate]]
        [clojure.tools.macro :only [macrolet]]))
(immigrate 'midje.checkers)


(defmulti ^{:private true} check-result (fn [actual call] 
                                          (:desired-check call)))

(letfn [(base-result [type actual call] 
          {:type type
           :binding-note (:binding-note call)
           :position (:position call)
           :actual actual })]

  (defn- failure [type actual call]
    (assoc (base-result type actual call) :expected (:expected-result-text-for-failures call)))

  (defn- success [type actual call]
    (assoc (base-result type actual call) :expected (:expected-result call) )))

(defmethod check-result :check-match [actual call]
  (cond (extended-= actual (:expected-result call))
        (report {:type :pass})

        (fn? (:expected-result call))
        (report (merge (failure :mock-expected-result-functional-failure actual call)
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
        (report (success :mock-expected-result-failure actual call))))

(defmethod check-result :check-negated-match [actual call]
   (cond (not (extended-= actual (:expected-result call)))
         (report {:type :pass})

        (fn? (:expected-result call))
        (report (failure :mock-actual-inappropriately-matches-checker actual call))

        :else
        (report (failure :mock-expected-result-inappropriately-matched actual call))))

(defn expect*
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  [unprocessed-check local-fakes]
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