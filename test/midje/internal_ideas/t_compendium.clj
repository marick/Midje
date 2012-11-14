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

(defn a-fact [name source]
  (let [starting-value
        (if name
          (with-meta [(str "this-vector-represents-a-faux-fact-named-" name)]
                     {:midje/name name})
          (with-meta ["some unnamed fact"]
                     {}))]
    (vary-meta starting-value
               merge {:midje/namespace common-namespace
                      :midje/true-name (gensym 'TRUENAME-)
                      :midje/source source})))

(def named (a-fact "named" '(source)))
(def unnamed (a-fact nil '(source 2)))
  

;;; Tests

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

(fact "entire namespaces' worth of facts can be forgotten"
  (let [compendium (-> (fresh-compendium)
                       (add-to named)
                       (remove-namespace-facts-from common-namespace))]
    (all-facts compendium) => empty?
    (namespace-facts compendium common-namespace) => empty?
    (named-fact compendium common-namespace (fact-name named)) => nil
    (sourced-fact compendium common-namespace (fact-source named)) => nil
    (ns-resolve fact-var-namespace (fact-true-name named)) => nil))


(letfn [(check [existing possible-match]
          (-> (fresh-compendium)
              (add-to existing)
              (previous-version possible-match)))]
  (tabular "previous version"
    (fact 
      (let [existing ?existing
            possible-match ?possible]
        (check existing possible-match) => ?expected))
    ?existing           ?possible                   ?expected

    ;; Can have same name but different source
    (a-fact "same-name" '(one source))
    (a-fact "same-name" '(different source))        existing

    ;; Can have no name but same source
    (a-fact nil '(same source))
    (a-fact nil '(same source))                      existing

    ;; Not fooled by different names and same source
    (a-fact "name1" '(same source))
    (a-fact "name2" '(same source))                  nil

    ;; An unnamed fact can't match a named one, even if same source
    (a-fact "name1" '(same source))
    (a-fact nil '(same source))                      nil
    
    ;; A same-sourced fact matches when a name has been added to a no-named version.
    ;; This lets you replace an unnamed fact by adding a name,
    ;; reloading it, then changing the source, then reloading again.
    (a-fact nil '(same source))
    (a-fact "name" '(same source))                   existing

    ;; Not fooled by different namespaces and same name
    (a-fact "name1" '(same-source))
    (vary-meta (a-fact "name1" '(same-source)) assoc :midje/namespace 'clojure.core)
                                                      nil
            
    ;; ... or different namespaces and same source
    (a-fact nil '(same-source))
    (vary-meta (a-fact nil '(same-source)) assoc :midje/namespace 'clojure.core)
                                                      nil
            
    ))
