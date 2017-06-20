;; It's probably unusual to use `:require :as` when defining a record that
;; implements a protocol, but doing that makes what's going on more clear.

(ns as-documentation.about-defrecord.using-as--plain-tests.specific
  (:require [midje.sweet :refer :all]
            [midje.util.ecosystem :as ecosystem]
            [as-documentation.about-defrecord.generic :as generic]))

(fact "The generic function exists - it's a bound var"
  (bound? #'generic/bump) => true)


;; Note that self-calls require qualification with the generic namespace.
;; #'bump and #'twice are not bound at this point. That makes for confusing
;; declarations like
;;
;;   (bump [this] (generic/bump this 1))
;;
;; Why doesn't the first instance of the token `bump` need namespace qualification? It's
;; really special-case processing by the macros that make up the defrecord/deftype
;; complex. You *can* use a qualified name if you like:
;;
;;   (generic/bump [this] (generic/bump this 1))
;;
;; ... and everything work the same. However, that's not idiomatic.

(defrecord Record [n]
  generic/Numerical
  (bump [_ by] (+ n by))
  (bump [this] (generic/bump this 1))
  (twice [this] (generic/bump this (:n this))))

(fact "Protocol functions are never bound here: only the var in the generic namespace has a binding"
  (find-var 'as-documentation.about-defrecord.generic/bump) => truthy
  (ecosystem/when-1-6+
   ;; This throws a null-pointer exception in older Clojures
   (find-var 'as-documentation.about-defrecord.generic/using-as--plain-tests.specific/bump) => falsey))

(fact "Therefore, even within this namespace uses of the record must use the generic function's var"
  (let [rec (->Record 3)]
    (generic/bump rec) => 4
    (generic/bump rec 5) => 8
    (generic/twice rec) => 6))
