(ns cake-midje.core
  (:use midje.sweet))

(println "\n+++ The following shows how 'cake midje' checks facts in source files.")
(println "+++ The failure is intentional.")
(fact
  (+ 1 2) => "I am a source-file fact.")
