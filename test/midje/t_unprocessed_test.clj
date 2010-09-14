(ns midje.t-unprocessed-test
  (:use clojure.test)
  (:use [midje.unprocessed] :reload-all)
  (:use [midje.test-util]))


;;; Everything is tested indirectly through semi-sweet, including some of
;;; the internals.
