(ns user.fus-facts
  (:use midje.sweet
        midje.test-util)
  (:require [midje.config :as config]))
        

(config/with-augmented-config {:visible-future true}
  (silent-fact "future facts don't interfere with the checking of other predictions"
    (+ 1 1) => 3
    (+ 11 "1") =future=> "12"  ;; does not fail
    (+ 111 1) => 112)
  (note-that (fails 1 time), (fact-actual 2), (fact-expected 3)))

  
