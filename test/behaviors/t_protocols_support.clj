;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-protocols-support)

(defprotocol RecordFakeable
  (fake-me [this function])
  (call-fake-me [this]))

(defrecord OutsideNSFakeableRecord [a b]
  RecordFakeable
  (fake-me [this f]
          (if (midje.fakes/function-tagged-as-fake? fake-me)
            (apply fake-me [this f])
            (list a b (f a))))
  (call-fake-me [this]
                (if (midje.fakes/function-tagged-as-fake? call-fake-me)
                  (apply call-fake-me [this])
                  (fake-me this inc))))

(defn outside-double-inc-fake-me [tc]
  (concat (fake-me tc inc) (fake-me tc inc)))
