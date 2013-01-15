(ns midje.ideas.reporting.t-string-format-map-translation
  (:use midje.ideas.reporting.report
        [midje.ideas.reporting.string-format :only [midje-position-string]]
        [midje.error-handling.exceptions :only [captured-throwable]]
        [midje sweet test-util]
        midje.util))

(expose-testables midje.ideas.reporting.string-format)


;; These tests generate failures to examine. We don't want them to be
;; added to the total failure count, which should always be zero.
(without-changing-cumulative-totals
    
)  ; end without-changing-cumulative-totals
