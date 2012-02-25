(ns ^{:cljs-ns 'midje.cljs.basic
      :cljs-file "midje/cljs/basic.cljs"}
  midje.cljs.t-basic
  (:use [midje.sweet]))

(fact '(doubler 5) => 10)