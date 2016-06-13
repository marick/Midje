(ns user.fus-protocols.protocols-defined-in-another-namespace
  (:require [midje.open-protocols :refer :all]))

(defprotocol RecordFakeable
  (fake-me [this function])
  (call-fake-me [this]))

(defrecord-openly OutsideNSFakeableRecord [a b]
  RecordFakeable
  (fake-me [this f] (list a b (f a)))
  (call-fake-me [this] (fake-me this inc)))

(defn outside-double-inc-fake-me [tc]
  (concat (fake-me tc inc) (fake-me tc inc)))

