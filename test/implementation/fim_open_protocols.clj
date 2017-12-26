(ns implementation.fim-open-protocols
  (:require [midje.open-protocols :refer :all]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.util :refer :all]))
(expose-testables midje.open-protocols)

(fact "can distinguish a protocol/interface name from a function implementation"
  (implementation? 'Object) => false
  (implementation? '(fake-me [this f] form1 form2)) => true)

(defprotocol P (f [this x]))

(fact "can distinguish a protocol from an interface"
  (protocol? 'Object) => false
  (protocol? 'java.lang.Comparable) => false
  (protocol? 'P) => true)


(let [type-or-record-tail '([a b c] P (f [this x] (+ x a b c)))
      in-typ             (list* 'deftype-openly 'T type-or-record-tail)
      unexpanded-out-typ (list* 'clojure.core/deftype        'T type-or-record-tail)
      in-rec             (list* 'defrecord-openly 'R type-or-record-tail)
      unexpanded-out-rec (list* 'clojure.core/defrecord        'R type-or-record-tail)]
  (fact "normally, types and records are rewritten"
    (macroexpand-1 in-typ) =not=> unexpanded-out-typ
    (macroexpand-1 in-rec) =not=> unexpanded-out-rec)

  (fact "but they remain the same when the user doesn't want checking"
    (try
      (alter-var-root #'midje.sweet/include-midje-checks (constantly false))
      (macroexpand-1 in-typ) => unexpanded-out-typ
      (macroexpand-1 in-rec) => unexpanded-out-rec
    (finally
     (alter-var-root #'midje.sweet/include-midje-checks (constantly true))))))




