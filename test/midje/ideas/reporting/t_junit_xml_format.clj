(ns midje.ideas.reporting.t-junit-xml-format
  (:use midje.sweet
        midje.test-util))

(future-fact "reported XML validates vs JUnit XML schema")