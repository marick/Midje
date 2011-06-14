;; -*- indent-tabs-mode: nil -*-

(ns midje.t-fact-table
  (:use [midje.sweet])
  (:use [midje.test-util]))

(fact-table
 (+ 1 1) => 2)


