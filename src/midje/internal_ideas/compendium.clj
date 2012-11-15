(ns ^{:doc "A compendium is 'a collection of concise but detailed information
            about a particular subject'. The Midje compendium contains
            the currently relevant facts."}
  midje.internal-ideas.compendium
  (:use [midje.ideas.metadata :only [fact-name fact-true-name
                                     fact-body-source fact-namespace]]
        [midje.util.form-utils :only [dissoc-keypath]]))

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
  (remove-namespace-facts-from [this namespace])
  (namespace-facts [this namespace])
  (all-facts [this])
  (named-fact [this namespace name])
  (embodied-fact [this namespace source])
  (previous-version [this fact-function]))

;; The compendium has three maps, each keyed by a namespace name.
;; 
;; One maps that namespace to a list of facts (in the order in which
;; the facts were defined, which is usually the order of facts in the file.
;; This map is sorted alphabetically.
;;
;; Another maps to a by-name map of facts for quick lookup by name.
;;
;; Another maps to a by-body-source map to allow quick checks for
;; reloading of identical facts.

(defrecord Compendium [by-namespace by-name by-source]
  CompendiumProtocol
  (add-to [this fact-function]
    (let [[namespace name body-source true-name]
          ( (juxt fact-namespace fact-name fact-body-source fact-true-name)
            fact-function)]
      (intern fact-var-namespace true-name fact-function)
      (-> this 
          (assoc-in [:by-namespace namespace]
                    (conj (by-namespace namespace []) fact-function))
          (#(if name
              (assoc-in % [:by-name namespace name] fact-function)
              %))
          (assoc-in [:by-source namespace body-source] fact-function))))


  (remove-from [this fact-function]
    (let [[namespace name body-source true-name]
          ( (juxt fact-namespace fact-name fact-body-source fact-true-name)
            fact-function)]
      (ns-unmap fact-var-namespace true-name)
      (-> this
          (assoc-in [:by-namespace namespace]
                    (remove #(= % fact-function) (by-namespace namespace)))
          (dissoc-keypath [:by-name namespace name])
          (dissoc-keypath [:by-source namespace body-source]))))

  (remove-namespace-facts-from [this namespace]
    (let [namespace-name (ns-name namespace)]
      (dorun (map #(ns-unmap fact-var-namespace (fact-true-name %))
                  (by-namespace namespace-name)))
      (-> this 
          (dissoc-keypath [:by-namespace namespace-name])
          (dissoc-keypath [:by-name namespace-name])
          (dissoc-keypath [:by-source namespace-name]))))

  (namespace-facts [this namespace]
    (get by-namespace (ns-name namespace) []))
  (all-facts [this]
    (apply concat (vals by-namespace)))
  (named-fact [this namespace name]
    (get-in by-name [(ns-name namespace) name]))
  (embodied-fact [this namespace body-source]
    (get-in by-source [(ns-name namespace) body-source]))

  (previous-version [this fact-function]
    (let [[namespace name body-source]
            ( (juxt fact-namespace fact-name fact-body-source) fact-function)
          existing-named-fact (named-fact this namespace name)
          existing-embodied-fact (embodied-fact this namespace body-source)]
      (cond existing-named-fact
            existing-named-fact

            (and existing-embodied-fact
                 (not (fact-name existing-embodied-fact)))
            existing-embodied-fact

            :else
            nil))))
  
(defn fresh-compendium []
  (Compendium. (sorted-map) {} {}))

