;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-strings
  (:use [midje.util.strings]
	[midje sweet test-util]))


(fact "strings can have newlines added"
  (newlineify ["a"]) => [(str "a" (System/getProperty "line.separator"))])

(fact "strings can have prefix added"
  (prefix ["a"] "bbb") => ["bbba"])
