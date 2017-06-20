(ns implementation.checking.fim-facts
  (:require [midje.checking.facts :as subject]
            [midje.config :as config]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [such.metadata :as meta]))


(def ^:dynamic record)

(def faux-fact #(reset! record "called"))

(defn top-level [v iob] (meta/assoc iob :midje/top-level-fact? v))
(defn tagged [key iob] (meta/assoc iob key true))

(defmacro with-record [& body]
  `(binding [record (atom "uncalled")]
     ~@body))

(defmacro with-filter [choices & body]
  `(config/with-augmented-config {:fact-filter (config/mkfn:fact-filter-predicate ~choices)}
     (with-record ~@body)))

(fact "check-one"
  (facts "top level"
    (fact "running a fact normally"
      (with-record
        (subject/check-one (top-level true faux-fact))
        @record => "called"))

    (fact "running a fact that matches a fact filter"
      (with-filter [:filt]
        (subject/check-one (tagged :filt (top-level true faux-fact)))
        @record => "called"))

    (fact "running a fact without metadata in context of filter: uncalled"
      (with-filter [:filt]
        (subject/check-one (top-level true faux-fact))
        @record => "uncalled"))

    (fact "running a fact with wrong metadata in context of filter: uncalled"
      (with-filter [:filt]
        (subject/check-one (tagged :mismatch (top-level true faux-fact)))
        @record => "uncalled")))

  (facts "lower level"
    (fact "running a fact normally"
      (with-record
        (subject/check-one (top-level false faux-fact))
        @record => "called"))

    (fact "running a fact without metadata in context of filter: *called*"
      ;; Called because if the filter mattered, this function would never be called.
      (with-filter [:filt]
        (subject/check-one (top-level false faux-fact))
        @record => "called"))

    (fact "running a fact with wrong metadata in context of filter: *called*"
      ;; Same reason as above.
      (with-filter [:filt]
        (subject/check-one (tagged :mismatch (top-level false faux-fact)))
        @record => "called"))))

