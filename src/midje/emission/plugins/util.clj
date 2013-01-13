(ns ^{:doc "General purpose plugin utilities"}
  midje.emission.plugins.util
  (:require [midje.clojure-test-facade :as ctf]))


;; The theory here was that using clojure.test output would allow text
;; from failing *facts* to appear within the clojure.test segment of
;; summary output. That doesn't work (though it works fine for
;; clojure.test output). The whole rigamarole is boring and I don't
;; care to jump through any more hoops. I say it's spinach, and I say
;; to hell with it.

;; (Don't change this to println, though. That doesn't work either.
;; Newlines are lost.)
(def output ctf/output)
