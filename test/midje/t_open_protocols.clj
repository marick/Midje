;; -*- indent-tabs-mode: nil -*-

(ns midje.t-open-protocols
  (:use midje.open-protocols)
  (:use [midje sweet test-util]))

;; The end-to-endish tests are under tests/behaviors.

(fact "can distinguish a protocol name from a function implementation"
  (#'midje.open-protocols/implementation? 'REDEEMABLE) => falsey
   (#'midje.open-protocols/implementation? '(fake-me [this f] form1 form2)) => truthy)


(defprotocol P (f [this x]))
(let [type-or-record-tail '([a b c] P (f [this x] (+ x a b c)))
      in-typ             (list* 'deftype-openly 'T type-or-record-tail)
      unexpanded-out-typ (list* 'deftype        'T type-or-record-tail)
      in-rec             (list* 'defrecord-openly 'R type-or-record-tail)
      unexpanded-out-rec (list* 'defrecord        'R type-or-record-tail)]
  (fact "normally, types and records are rewritten"
    (macroexpand-1 in-typ) =not=> unexpanded-out-typ
    (macroexpand-1 in-rec) =not=> unexpanded-out-rec

    "but they remain the same when the user doesn't want checking"
    (with-bindings {#'midje.sweet/*include-midje-checks* false}
      (macroexpand-1 in-typ) => unexpanded-out-typ
      (macroexpand-1 in-rec) => unexpanded-out-rec)))
      



