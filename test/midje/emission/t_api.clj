(ns midje.emission.t-api
  (:use midje.sweet
        midje.test-util)
  (:require [midje.emission.api :as emit]
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
      @state/output-counters)))

(fact emit/pass
  (innocuously
   (config/at-print-level :print-nothing (emit/pass))
   (config/at-print-level (levels/level-above :print-nothing) (emit/pass)))
  => (contains {:midje-passes 2})
  (plugin/recorded) => [[:pass]])

(fact emit/fail
  (innocuously
   (config/at-print-level :print-nothing (emit/fail ..ignored-failure-map..))
   (config/at-print-level (levels/level-above :print-nothing) (emit/fail ..failure-map..)))
  => (contains {:midje-failures 2})
  (plugin/recorded) => [[:fail ..failure-map..]])
     
(fact emit/starting-to-check-fact
  (innocuously
    (config/at-print-level (levels/level-below :print-facts) (emit/starting-to-check-fact ..ignored-fact-function..))
    (config/at-print-level :print-facts (emit/starting-to-check-fact ..fact-function..)))
  (plugin/recorded) => [[:starting-to-check-fact ..fact-function..]])


(fact emit/possible-new-namespace
  (innocuously
    (config/at-print-level (levels/level-below :print-namespaces) (emit/possible-new-namespace ..ignored..))
    (config/at-print-level :print-namespaces (emit/possible-new-namespace ..emitted..)))
  (plugin/recorded) => [[:possible-new-namespace ..emitted..]])
