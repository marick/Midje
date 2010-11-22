(ns midje.util.t-thread-safe-var-nesting
  (:use [midje.util.thread-safe-var-nesting] :reload-all)
  (:use midje.sweet)
  (:require [clojure.zip :as zip])
  (:use midje.test-util)
)

(def root "root")
(def unbound!)

(facts "altered roots.."
  "... nest"
  root => "root"
  (with-altered-roots {(var root) "override"} root) => "override"
  root => "root"

  "... work with lexical scope"
  (let [appendage " ROOT!"]
    (with-altered-roots {#'root "override"} (str root appendage)) => "override ROOT!"
    root => "root")
  root => "root"

  "... work with unbound variables"
  (bound? #'unbound!) => falsey
  (with-altered-roots {#'unbound! "override"} unbound!) => "override"
  (bound? #'unbound!) => falsey

  "... work with throwables"
  (try
    (with-altered-roots {#'root "override"} (throw (Throwable. root)))
    (catch Throwable ex
      (.getMessage ex) => "override"))
  root => "root")

