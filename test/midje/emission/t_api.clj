(ns midje.emission.t-api
  (:use midje.sweet
        midje.test-util)
  (:require [midje.emission.api :as emit]))


(fact load-plugin
  (fact "symbols are required"
    (emit/load-plugin 'symbol) => irrelevant
    (provided
      (require 'symbol :reload) => anything))

  (fact "strings are given to load-file"
    (emit/load-plugin "string") => irrelevant
    (provided
      (load-file "string") => anything)))
      
    

