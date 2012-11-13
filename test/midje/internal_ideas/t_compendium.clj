(ns midje.internal-ideas.t-compendium
  (:use [midje.sweet]
        [clojure.pprint]
        [midje.test-util]
        [midje.internal-ideas.compendium]
        [midje.ideas.metadata :only [fact-name fact-true-name
                                     fact-source]])
  (:import midje.internal_ideas.compendium.Compendium))

;; Some faux facts to use in tests

(def common-namespace 'midje.sweet)

(def named (with-meta ['this-vector-represents-a-named-faux-fact]
                 {:midje/namespace common-namespace
                  :midje/name "name"
                  :midje/true-name 'TRUENAME
                  :midje/source '(source)}))
(def unnamed (with-meta ['this-vector-represents-an-unnamed-faux-fact]
                   {:midje/namespace common-namespace
                    ;; No name
                    :midje/true-name 'TRUENAME2
                    :midje/source '(source2)}))


;;; The core data structure

(fact "an empty compendium"
  (all-facts (fresh-compendium)) => empty?)

(fact "adding a fact to the compendium"
  (let [compendium (-> (fresh-compendium)
                       (add-to named))]
    (all-facts compendium) => [named]
    (namespace-facts compendium common-namespace) => [named]
    (named-fact compendium common-namespace (fact-name named)) => named
    (sourced-fact compendium common-namespace (fact-source named)) => named
    (deref (ns-resolve fact-var-namespace (fact-true-name named))) => named

    (fact "adds facts in order"
      (namespace-facts (add-to compendium unnamed) common-namespace)
      => [named unnamed])

    (fact "can include an unnamed fact"
      (let [compendium (add-to compendium unnamed)]
        (all-facts compendium) => [named unnamed]
        (namespace-facts compendium common-namespace) => [named unnamed]
        (named-fact compendium common-namespace (fact-name unnamed)) => nil
        (sourced-fact compendium common-namespace (fact-source unnamed)) => unnamed
        (deref (ns-resolve fact-var-namespace (fact-true-name unnamed))) => unnamed))))

(fact "when namespaces are called for, they can be a symbol"
  (let [compendium (-> (fresh-compendium)
                       (add-to named))
        true-namespace (the-ns common-namespace)
        symbol-namespace (ns-name true-namespace)]
    (namespace-facts compendium true-namespace) => [named]
    (named-fact compendium true-namespace (fact-name named)) => named
    (sourced-fact compendium true-namespace (fact-source named)) => named

    (namespace-facts compendium symbol-namespace) => [named]
    (named-fact compendium symbol-namespace (fact-name named)) => named
    (sourced-fact compendium symbol-namespace (fact-source named)) => named))

    
(fact "deleting from the compendium"
  (let [compendium (-> (fresh-compendium)
                       (add-to named)
                       (add-to unnamed))]

    (all-facts compendium) => [named unnamed]
    (namespace-facts compendium common-namespace) => [named unnamed]
    (named-fact compendium common-namespace (fact-name named)) => named
    (named-fact compendium common-namespace (fact-name unnamed)) => nil
    (sourced-fact compendium common-namespace (fact-source named)) => named
    (sourced-fact compendium common-namespace (fact-source unnamed)) => unnamed

    (fact "deletes named facts"
      (let [result (remove-from compendium named)]
        (all-facts result) => [unnamed]
        (namespace-facts result common-namespace) => [unnamed]
        (named-fact result common-namespace (fact-name named)) => nil
        (sourced-fact result common-namespace (fact-source named)) => nil
        (ns-resolve fact-var-namespace (fact-true-name named)) => nil))

    (fact "also deletes unnamed facts"
      (let [result (remove-from compendium unnamed)]
        (all-facts result) => [named]
        (namespace-facts result common-namespace) => [named]
        (sourced-fact result common-namespace (fact-source unnamed)) => nil
        (ns-resolve fact-var-namespace (fact-true-name unnamed)) => nil))))


