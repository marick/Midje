(ns ^{:doc "A compendium is 'a collection of concise but detailed information
            about a particular subject'. The Midje compendium contains
            the currently relevant facts."}
  midje.internal-ideas.compendium
  (:use [midje.ideas.metadata :only [fact-name fact-true-name
                                     fact-source fact-namespace]]
        [midje.util.form-utils :only [dissoc-keypath]]))
(declare assoc-in-ignoring-nil)


;;; Facts are referred to by vars in a namespace


(def fact-var-namespace
  (do (in-ns 'midje.fact-var-namespace)
      (let [retval *ns*]
        (in-ns 'midje.internal-ideas.compendium)
        retval)))

;;; Facts are stored in a compendium:

(defprotocol CompendiumProtocol
  (add-to [this fact-function])
  (remove-from [this fact-function])
  (namespace-facts [this namespace])
  (all-facts [this])
  (named-fact [this namespace name])
  (sourced-fact [this namespace source]))

(defrecord Compendium [by-namespace by-name by-source]
  CompendiumProtocol
  (add-to [this fact-function]
    (let [[namespace name source true-name]
          ( (juxt fact-namespace fact-name fact-source fact-true-name)
            fact-function)

          new-namespace-facts
          (conj (by-namespace namespace []) fact-function)]

      (letfn [(assoc-pair [map submap-key key]
                (if key
                  (assoc-in map [submap-key [namespace key]] fact-function)
                  map))]
        (intern fact-var-namespace true-name fact-function)
        (-> this 
            (assoc-in [:by-namespace namespace] new-namespace-facts)
            (assoc-pair :by-name name)
            (assoc-pair :by-source source)))))


  (remove-from [this fact-function]
    (let [[namespace name source true-name]
          ( (juxt fact-namespace fact-name fact-source fact-true-name)
            fact-function)

          new-namespace-facts
          (remove (partial = fact-function) (by-namespace namespace))]
      
      (ns-unmap fact-var-namespace true-name)
      (-> this 
          (assoc-in [:by-namespace namespace] new-namespace-facts)
          (dissoc-keypath [:by-name [namespace name]])
          (dissoc-keypath [:by-source [namespace source]]))))

  (namespace-facts [this namespace]
    (get by-namespace (ns-name namespace)))
  (all-facts [this]
    (apply concat (vals by-namespace)))
  (named-fact [this namespace name]
    (get by-name [(ns-name namespace) name]))
  (sourced-fact [this namespace source]
    (get by-source [(ns-name namespace) source])))
  
(defn fresh-compendium []
  (Compendium. (sorted-map) {} {}))



;;;; UTIL

(defn- assoc-in-ignoring-nil [map keys value]
  (if (nil? (last keys))
    map
    (assoc-in map keys value)))

