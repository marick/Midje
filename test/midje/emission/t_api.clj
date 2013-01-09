(ns midje.emission.t-api
  (:use midje.sweet
        midje.test-util)
  (:require [midje.emission.api :as emit]
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



  (fact emit/pass
    (without-changing-cumulative-totals     
     (state/reset-output-counters!)

     (state/with-emission-map plugin/emission-map
       (config/with-augmented-config {:print-level :print-nothing} (emit/pass)) => anything
       (provided
         (plugin/pass) => irrelevant :times 0)
     
       (config/with-augmented-config {:print-level :print-no-summary} (emit/pass)) => anything
       (provided
         (plugin/pass) => irrelevant))
     
     (state/output-counters:midje-passes) => 4)) ;; This will soon become 2. It double-counts passes in api.clj

  (fact emit/fail
    (without-changing-cumulative-totals     
     (state/reset-output-counters!)

     (state/with-emission-map plugin/emission-map
       (config/with-augmented-config {:print-level :print-nothing} (emit/fail ..failure-map..)) => anything
       (provided
         (plugin/fail ..failure-map..) => irrelevant :times 0)
     
       (config/with-augmented-config {:print-level :print-no-summary} (emit/fail ..failure-map..)) => anything
       (provided
         (plugin/fail ..failure-map..) => irrelevant))
     
     (state/output-counters:midje-failures) => 2))


