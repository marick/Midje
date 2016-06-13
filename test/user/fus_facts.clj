(ns user.fus-facts
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.config :as config]))
        

(config/with-augmented-config {:visible-future true}
  (silent-fact "checkables marked 'future' don't interfere with the checking of other checkables"
    (+ 1 1) => 3
    (+ 11 "1") =future=> "12"  ;; does not fail
    (+ 111 1) => 112)
  (note-that (fails 1 time), (fact-actual 2), (fact-expected 3)))

  
