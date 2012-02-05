(ns ^{:cljs-ns 'midje.cljs.basic
      :cljs-file "midje/cljs/basic.cljs"}
  midje.cljs.t-basic
  (:use [midje.sweet]))

(binding [*cljs-ns-under-test* 'midje.cljs.basic
          *cljs-file-under-test* "midje/cljs/basic.cljs"]
  (fact 
    (doubler 5) => 10))