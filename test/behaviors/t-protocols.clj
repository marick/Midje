;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-canary
  (:use [midje.sweet])
  (:use [midje.test-util]))


(future-fact "... working on faking records")


(defprotocol RecordFakeable
  (fake-me [this function])
  (call-fake-me [this]))

(defrecord InNSFakeableRecord [a b]
  RecordFakeable
  (fake-me [this f]
          (if (midje.fakes/function-tagged-as-fake? fake-me)
            (apply fake-me [this f])
            (list a b (f a))))
  (call-fake-me [this]
                (if (midje.fakes/function-tagged-as-fake? call-fake-me)
                  (apply call-fake-me [this])
                  (fake-me this inc))))

(defn double-inc-fake-me [tc]
  (concat (fake-me tc inc) (fake-me tc inc)))


(fact "Record functions can be faked when called from outside"
  (let [rec (InNSFakeableRecord. 1 3)]
    (fake-me rec inc) => [1 3 2]
    (double-inc-fake-me rec) => [1 3 2 1 3 2]
    (double-inc-fake-me rec) => [1 1]
    (provided
      (fake-me rec inc) => [1])))

(fact "Record functions can be faked when called from inside"
  (let [rec (InNSFakeableRecord. 1 3)]
    (call-fake-me rec) => [1 3 3]
    (call-fake-me rec) => :faked
    (provided
      (fake-me rec inc) => :faked)))

