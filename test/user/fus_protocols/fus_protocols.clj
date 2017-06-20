(ns user.fus-protocols.fus-protocols
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.open-protocols :refer :all]
            [midje.util.ecosystem :refer :all]
            [user.fus-protocols.protocols-defined-in-another-namespace :refer :all]))
(import 'user.fus_protocols.protocols_defined_in_another_namespace.OutsideNSFakeableRecord)

(fact "Imported record functions can be faked when called from outside"
  (let [rec (OutsideNSFakeableRecord. 1 3)]
    (fake-me rec inc) => [1 3 2]
    (outside-double-inc-fake-me rec) => [1 3 2 1 3 2]
    (outside-double-inc-fake-me rec) => [1 1]
    (provided
      (fake-me rec inc) => [1])))

(fact "Imported record functions can be faked when called from inside"
  (let [rec (OutsideNSFakeableRecord. 1 3)]
    (call-fake-me rec) => [1 3 2]
    (call-fake-me rec) => :faked
    (provided
      (fake-me rec inc) => :faked)))

(defrecord-openly InNSFakeableRecord [a b]
  RecordFakeable
  (fake-me [this f] (list a b (f a)))
  (call-fake-me [this] (fake-me this inc)))

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
    (call-fake-me rec) => [1 3 2]
    (call-fake-me rec) => :faked
    (provided
      (fake-me rec inc) => :faked)))


;;; Works with more than one protocol

(defprotocol Addable
  (add-fields [this]))

(defprotocol MoreAddable
  (add-fields-and [this x]))

(deftype-openly LongerRecord [a b c]
  Addable
  (add-fields [this] (+ a b c))
  MoreAddable
  (add-fields-and [this x]
                  (+ (add-fields this) x)))

(fact
  (let [rec (LongerRecord. 1 2 3)]
    (add-fields rec) => 6
    (add-fields-and rec 7) => 13
    (add-fields-and rec 7) => 7
    (provided
      (add-fields rec) => 0)))

