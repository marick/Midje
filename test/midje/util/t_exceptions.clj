;; -*- indent-tabs-mode: nil -*-

(ns midje.util.t-exceptions
  (:use [midje.util.exceptions]
	[midje sweet test-util]
	[clojure.pprint]))

(def clojure-spewage-regexp #"^clojure\..*\(core.clj:\d+\)")

(fact "stacktraces can be fetched as strings"
  (stacktrace-as-strings (Throwable.)) => (contains clojure-spewage-regexp))

(fact "clojure spewage can be removed"
  (let [strings (relevant-strings-from-stacktrace (Throwable.))]
    (filter #(re-find clojure-spewage-regexp %) strings)) => empty?)

(fact "there is a format for printing exceptions"
  (let [lines (friendly-exception-lines (Error. "message") ">>>")]
    (first lines) => #"Error.*message"
    (re-find #"^>>>" (first lines)) => falsey
    (empty? (rest lines)) => falsey
    (filter #(re-find clojure-spewage-regexp %) (rest lines)) => empty?
    (re-find #"^>>>" (second lines)) => truthy
    ))
  
 
