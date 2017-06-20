(ns midje.data.t-compendium
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.data.compendium :refer :all]
            [midje.util.ecosystem :as ecosystem]
            [midje.config :as config]
            [midje.data.fact :as fact]
            [midje.checking.facts :as fact-checking]))
(import 'midje.data.compendium.Compendium)

;; Some faux facts to use in tests

(def common-namespace 'midje.sweet)

(defn a-fact [name source]
  (let [starting-value
        (if name
          (with-meta [(str "this-vector-represents-a-faux-fact-named-" name)]
                     {:midje/name name})
          (with-meta ["some unnamed fact"]
                     {}))]
    (vary-meta starting-value
               merge {:midje/namespace common-namespace
                      :midje/source source
                      :midje/guid (rest source)})))

(def named (a-fact "named" '(fact (+ 1 1) => 2)))
(def unnamed (a-fact nil '(fact 3 => odd?)))


;;; The direct functions

(fact "an empty compendium"
  (let [compendium (fresh)]
    (all-facts compendium) => empty?
;    ((:last-fact-checked compendium)) => "No fact has been checked."
    (namespace-facts compendium common-namespace) => empty?
    (named-fact compendium common-namespace (fact/name named)) => nil
    (fact-with-guid compendium common-namespace (fact/guid named)) => nil))

(fact "adding a fact to the compendium"
  (let [compendium (-> (fresh)
                       (add-to named))]
    (all-facts compendium) => [named]
;    ((:last-fact-checked compendium)) => "No fact has been checked."
    (named-fact compendium common-namespace (fact/name named)) => named
    (fact-with-guid compendium common-namespace (fact/guid named)) => named

    (fact "adds facts in order"
      (namespace-facts (add-to compendium unnamed) common-namespace)
      => [named unnamed])

    (fact "can include an unnamed fact"
      (let [compendium (add-to compendium unnamed)]
        (all-facts compendium) => [named unnamed]
        (namespace-facts compendium common-namespace) => [named unnamed]
        (named-fact compendium common-namespace (fact/name unnamed)) => nil
        (fact-with-guid compendium common-namespace (fact/guid unnamed)) => unnamed))))

(fact "when namespaces are called for, they can be a symbol"
  (let [compendium (-> (fresh)
                       (add-to named))
        true-namespace (the-ns common-namespace)
        symbol-namespace (ns-name true-namespace)]
    (namespace-facts compendium true-namespace) => [named]
    (named-fact compendium true-namespace (fact/name named)) => named
    (fact-with-guid compendium true-namespace (fact/guid named)) => named

    (namespace-facts compendium symbol-namespace) => [named]
    (named-fact compendium symbol-namespace (fact/name named)) => named
    (fact-with-guid compendium symbol-namespace (fact/guid named)) => named))


(fact "there is sometimes a useful error message when the symbol does not name a loaded namespace"
  (namespace-facts (fresh) 'no-such-namespace)
  => (throws Error #"namespace.*never been loaded")

  (named-fact (fresh) 'no-such-namespace "something")
  => (throws Error #"no-such-namespace.*never been loaded"))



(fact "deleting from the compendium"
  ;; Note: even if the last fact checked is deleted from the compendium,
  ;; it remains the last-fact-checked. (More strictly: I'm leaving the behavior
  ;; undefined.
  (let [compendium (-> (fresh)
                       (add-to named)
                       (add-to unnamed))]

    (all-facts compendium) => [named unnamed]
    (namespace-facts compendium common-namespace) => [named unnamed]
    (named-fact compendium common-namespace (fact/name named)) => named
    (named-fact compendium common-namespace (fact/name unnamed)) => nil
    (fact-with-guid compendium common-namespace (fact/guid named)) => named
    (fact-with-guid compendium common-namespace (fact/guid unnamed)) => unnamed

    (fact "deletes named facts"
      (let [result (remove-from compendium named)]
        (all-facts result) => [unnamed]
        (namespace-facts result common-namespace) => [unnamed]
        (named-fact result common-namespace (fact/name named)) => nil
        (fact-with-guid result common-namespace (fact/guid named)) => nil))

    (fact "also deletes unnamed facts"
      (let [result (remove-from compendium unnamed)]
        (all-facts result) => [named]
        (namespace-facts result common-namespace) => [named]
        (fact-with-guid result common-namespace (fact/guid unnamed)) => nil))))

(fact "forgetting an entire namespaces' worth of facts"
  (fact "can use a namespace name"
    (let [compendium (-> (fresh)
                         (add-to named)
                         (remove-namespace-facts-from common-namespace))]
      (all-facts compendium) => empty?
      (namespace-facts compendium common-namespace) => empty?
      (named-fact compendium common-namespace (fact/name named)) => nil
      (fact-with-guid compendium common-namespace (fact/guid named)) => nil))
  (fact "can use a namespace itself"
    (let [compendium (-> (fresh)
                         (add-to named)
                         (remove-namespace-facts-from (the-ns common-namespace)))]
      (all-facts compendium) => empty?
      (namespace-facts compendium common-namespace) => empty?
      (named-fact compendium common-namespace (fact/name named)) => nil
      (fact-with-guid compendium common-namespace (fact/guid named)) => nil))
  (fact "it is ok if the compendium doesn't exist"
    (remove-namespace-facts-from (fresh) 'some.imaginary.namespace)))


(letfn [(check [existing possible-match]
          (-> (fresh)
              (add-to existing)
              (previous-version possible-match)))]
  (tabular "previous version"
    (fact
      (let [existing ?existing
            possible-match ?possible]
        (check existing possible-match) => ?expected))
    ?existing           ?possible                   ?expected

    ;; Can have same name but different source
    (a-fact "same-name" '(fact one source))
    (a-fact "same-name" '(fact different source))        existing

    ;; Can have no name but same source
    (a-fact nil '(fact same source))
    (a-fact nil '(fact same source))                      existing

    ;; Not fooled by different names and same source
    (a-fact "name1" '(fact same source))
    (a-fact "name2" '(fact same source))                  nil

    ;; A same-sourced fact matches when a name has been added to a no-named version.
    ;; This lets you replace an unnamed fact by adding a name,
    ;; reloading it, then changing the source, then reloading again.
    (a-fact nil '(fact same source))
    (a-fact "name" '(fact same source))                  existing

    ;; An unnamed fact can't match a named one, even if same source
    ;; (which ought to be impossible)
    (a-fact "name1" '(fact same source))
    (a-fact nil '(fact same source))                      nil

    ;; Not fooled by different namespaces and same name
    (a-fact "name1" '(fact same-source))
    (vary-meta (a-fact "name1" '(fact same-source)) assoc :midje/namespace 'clojure.core)
                                                          nil

    ;; ... or different namespaces and same source
    (a-fact nil '(fact same-source))
    (vary-meta (a-fact nil '(fact same-source)) assoc :midje/namespace 'clojure.core)
                                                          nil

    ))

;; The functions that work on global state

(with-isolated-compendium
  ;; Certain facts do not allow themselves to be recorded.
  (fact :check-only-at-load-time (str "foo") => "foo")
  (fact :check-only-at-load-time
    (all-facts<>) => empty?
    ((last-fact-checked<>)) => "No fact has been checked.")

  ;; The user can also override creation-time recording (but not run-time)
  (config/with-augmented-config {:fact-filter (constantly false)}
    (fact (str "foo" "bar") => "foobar")
    (fact :check-only-at-load-time
      (all-facts<>) => empty?
      (fact-checking/check-one (last-fact-checked<>)) => true)))



