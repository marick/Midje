;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-default-prerequisites
  (:use [midje sweet test-util]
        [midje.internal-ideas.fakes :only [tag-as-background-fake]]))

(defn calls-nothing [])
(unfinished unused)

(fact "background prerequisites don't have to be used"
  (expect (calls-nothing) => nil
          (tag-as-background-fake (fake (unused) => 3 :type :background))))

