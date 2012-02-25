(ns midje.t-cljs
  (:require [midje.cljs :as base])
  (:use [midje.sweet :as sweet]))

(fact
 (do (base/load-cljs "midje/cljs/basic.cljs")
     (base/cljs-eval '(doubler 4) 'midje.cljs.basic))
 => 8)
