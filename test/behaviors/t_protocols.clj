;; -*- indent-tabs-mode: nil -*-

(ns behaviors.t-protocols
  (:use [midje sweet test-util open-protocols]
        behaviors.t-protocols-support)
  (:import behaviors.t-protocols-support.OutsideNSFakeableRecord))

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


(defprotocol Addable
  (add-fields [this]))
    
(defrecord-openly MyRecord [a b]
  Addable
  (add-fields [this] (+ a b)))

(defn indirect-adder [a b]
  (add-fields (MyRecord. a b)))

(fact
  (indirect-adder 1 2) => 3
  (indirect-adder 1 2) => :faked!
  (provided
    (add-fields anything) => :faked!))
    
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





;; An example used in the user documentation
(defprotocol Peanoific
  (pzero? [this])
  (pequal? [this that])
  (psuccessor [this])
  (padd [this that])
  (pmult [this that]))

(defrecord-openly Peano [value]
  Peanoific
  (pzero? [this] :unfinished)
  (pequal? [this that] :unfinished)
  (psuccessor [this] :unfinished)
  (padd [this that]
        (if (pzero? that) this))
  (pmult [this that] :unfinished))


(fact
  (padd (Peano. ...n...) (Peano. ...zero...)) => (Peano. ...n...)
  (provided
    (pzero? (Peano. ...zero...)) => true))

(after-silently 
 (fact 
   (padd (Peano. ...a...) (psuccessor (Peano. ...b...)))
   => (psuccessor (padd (Peano. ...a...) (Peano. ...b...)))
   (provided
     (pzero? anything) => false))
 (fact @reported => (one-of bad-result)))


