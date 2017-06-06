(ns midje.emission.t-api
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.emission.api :as emit]
            [midje.emission.levels :as levels]
            [midje.emission.state :as state]
            [midje.emission.plugins.test-support :as plugin]
            [midje.config :as config]))

(fact load-plugin
  (fact "symbols are required"
    (emit/load-plugin 'symbol) => irrelevant
    (provided
      (require 'symbol :reload) => anything))

  (fact "strings are given to load-file"
    (emit/load-plugin "string") => irrelevant
    (provided
      (load-file "string") => anything)))




(defmacro innocuously [& body]
  `(without-changing-cumulative-totals
    (state/with-emission-map plugin/emission-map
      (state/reset-output-counters!)
      (plugin/reset-recorder!)
      ~@body
      (state/output-counters))))

(fact emit/pass
  (innocuously
   (config/at-print-level :print-nothing (emit/pass))
   (config/at-print-level (levels/level-above :print-nothing) (emit/pass)))
  => (contains {:midje-passes 2})
  (plugin/recorded) => [[:pass]])

(fact emit/future-fact
  (config/with-augmented-config {:visible-future :true}
    (innocuously
     (config/at-print-level :print-nothing (emit/future-fact ..ignored.. ..ignored..))
     (config/with-augmented-config {:visible-future false,:print-level :print-facts}
       (emit/future-fact ..also-ignored.. ..also-ignored..))
     (config/at-print-level (levels/level-above :print-nothing) (emit/future-fact ..description.. ..position..))))
  (plugin/recorded) => [[:future-fact ..description.. ..position..]])

(fact emit/fail
  (innocuously
   (config/at-print-level :print-nothing (emit/fail ..ignored-failure-map..))
   (config/at-print-level (levels/level-above :print-nothing) (emit/fail ..failure-map..)))
  => (contains {:midje-failures 2})
  (plugin/recorded) => [[:fail ..failure-map..]])

(fact emit/starting-to-check-top-level-fact
  (innocuously
    (config/at-print-level (levels/level-below :print-facts) (emit/starting-to-check-top-level-fact ..ignored-fact-function..))
    (config/at-print-level :print-facts (emit/starting-to-check-top-level-fact ..fact-function..)))
  (plugin/recorded) => [[:starting-to-check-top-level-fact ..fact-function..]])

(fact emit/finishing-top-level-fact
  (innocuously
    (config/at-print-level (levels/level-below :print-facts) (emit/finishing-top-level-fact ..ignored-fact-function..))
    (config/at-print-level :print-facts (emit/finishing-top-level-fact ..fact-function..)))
  (plugin/recorded) => [[:finishing-top-level-fact ..fact-function..]])

(fact emit/starting-to-check-fact
  (innocuously
    (config/at-print-level (levels/level-below :print-facts) (emit/starting-to-check-fact ..ignored-fact-function..))
    (config/at-print-level :print-facts (emit/starting-to-check-fact ..fact-function..)))
  (plugin/recorded) => [[:starting-to-check-fact ..fact-function..]])

(fact emit/finishing-fact
  (innocuously
    (config/at-print-level (levels/level-below :print-facts) (emit/finishing-fact ..ignored-fact-function..))
    (config/at-print-level :print-facts (emit/finishing-fact ..fact-function..)))
  (plugin/recorded) => [[:finishing-fact ..fact-function..]])

(fact emit/possible-new-namespace
  (innocuously
    (config/at-print-level (levels/level-below :print-namespaces) (emit/possible-new-namespace ..ignored..))
    (config/at-print-level :print-namespaces (emit/possible-new-namespace ..emitted..)))
  (plugin/recorded) => [[:possible-new-namespace ..emitted..]])

(fact emit/fact-stream-summary
  (let [output-counters {:midje-passes 2
                         :midje-failures 3}
        clojure-test-results {:test 6
                              :pass 1
                              :fail 2
                              :error 3
                              :lines ["line"]}]
    ;; No output
    (innocuously (config/at-print-level :print-no-summary
        (emit/fact-stream-summary)))
    (plugin/recorded) => []

    ;; Has fact-stream summary
    (innocuously (config/at-print-level (levels/level-above :print-no-summary)
       (state/set-output-counters! output-counters)
       (emit/fact-stream-summary clojure-test-results)))
    (plugin/recorded) => [[:finishing-fact-stream output-counters clojure-test-results]]


    ;; No clojure.test run, so fact results.
    (innocuously (config/at-print-level (levels/level-above :print-no-summary)
      (state/set-output-counters! output-counters)
      (emit/fact-stream-summary)))
    (plugin/recorded) => [[:finishing-fact-stream output-counters {:test 0}]]))
