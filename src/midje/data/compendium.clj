(ns ^{:doc "A compendium is 'a collection of concise but detailed information
            about a particular subject'. The Midje compendium contains
            the currently relevant facts."}
  midje.data.compendium
  (:require [midje.config :as config]
            [midje.data.fact :as fact]
            [midje.util.exceptions :refer [user-error]]
            [such.maps :as map]))

;;; Facts are stored in a compendium:

(defprotocol CompendiumProtocol
  (add-to [this fact-function])
  (remove-from [this fact-function])
  (remove-namespace-facts-from [this namespace])
  (namespace-facts [this namespace])
  (all-facts [this])
  (named-fact [this namespace name])
  (fact-with-guid [this namespace guid])
  (previous-version [this fact-function]))


;; The compendium has three maps, each keyed by a namespace name.
;;
;; One maps that namespace to a list of facts (in the order in which
;; the facts were defined, which is usually the order of facts in the file.
;; This map is sorted alphabetically.
;;
;; Another maps to a by-name map of facts for quick lookup by name.
;;
;; Another maps to a body-guid map to allow quick checks for
;; reloading of identical facts.

(defn friendly-ns-name [symbol-or-namespace]
  (when (and (symbol? symbol-or-namespace)
             (not (find-ns symbol-or-namespace)))
    (throw (user-error (str "You tried to work with `" symbol-or-namespace
                             "` but that namespace has never been loaded."))))
  (ns-name symbol-or-namespace))



(defrecord Compendium [by-namespace by-name by-guid last-fact-checked]
  CompendiumProtocol
  (add-to [this fact-function]
    (let [[namespace name guid]
          ( (juxt fact/namespace fact/name fact/guid)
            fact-function)]
      (-> this
          (assoc-in [:by-namespace namespace]
                    (conj (by-namespace namespace []) fact-function))
          (#(if name
              (assoc-in % [:by-name namespace name] fact-function)
              %))
          (assoc-in [:by-guid namespace guid] fact-function))))


  (remove-from [this fact-function]
    (letfn [(vector-remove [vector target]
              (let [index-to-exclude (.indexOf ^clojure.lang.PersistentVector vector target)]
                  (assert (not (neg? index-to-exclude)))
                  (into (subvec vector 0 index-to-exclude)
                        (subvec vector (inc index-to-exclude)))))]
      (let [[namespace name guid]
            ( (juxt fact/namespace fact/name fact/guid)
              fact-function)

            new-namespace-facts
            (vector-remove (by-namespace namespace) fact-function)]
      (-> this
          (assoc-in [:by-namespace namespace] new-namespace-facts)
          (map/dissoc-keypath [:by-name namespace name])
          (map/dissoc-keypath [:by-guid namespace guid])))))

  (remove-namespace-facts-from [this namespace]
    (if (and (symbol? namespace)
             (not (find-ns namespace)))
      this
      (let [namespace-name (friendly-ns-name namespace)]
        (-> this
            (map/dissoc-keypath [:by-namespace namespace-name])
            (map/dissoc-keypath [:by-name namespace-name])
            (map/dissoc-keypath [:by-guid namespace-name])))))

  (namespace-facts [this namespace]
    (get by-namespace (friendly-ns-name namespace) []))
  (all-facts [this]
    (apply concat (vals by-namespace)))
  (named-fact [this namespace name]
    (get-in by-name [(friendly-ns-name namespace) name]))
  (fact-with-guid [this namespace guid]
    (get-in by-guid [(friendly-ns-name namespace) guid]))

  (previous-version [this fact-function]
    (let [[namespace name guid]
            ( (juxt fact/namespace fact/name fact/guid) fact-function)
          existing-named-fact (named-fact this namespace name)
          existing-fact-with-guid (fact-with-guid this namespace guid)]
      (cond existing-named-fact
            existing-named-fact

            (and existing-fact-with-guid
                 (not (fact/name existing-fact-with-guid)))
            existing-fact-with-guid

            :else
            nil))))

(defn fresh []
  (Compendium. (sorted-map) {} {}
               (fn [] "No fact has been checked.")))

;;; Functions on the mutable compendium.
;;; Functions that change the state are marked with !
;;; Functions that refer to it are marked with <> for no particular reason.

(def global (atom (fresh)))

(defn fresh! []
  (reset! global (fresh))
  true)

(defmacro with-isolated-compendium [& body]
  `(let [current-compendium# @global]
     (try
       (fresh!)
       ~@body
     (finally
      (reset! global current-compendium#)))))

(defn record-fact-check! [function]
  (when (fact/allows-itself-to-be-recorded? function)
    (swap! global assoc :last-fact-checked function)))

(defn record-fact-existence! [fact-function]
  (when (and (fact/allows-itself-to-be-recorded? fact-function)
             (config/user-wants-fact-to-be-recorded? fact-function))
    (swap! global #(if-let [previous (previous-version % fact-function)]
                     (remove-from % previous)
                     %))
    (swap! global add-to fact-function))
  ;; Returning the fact-function is a kludge required by the
  ;; way tabular facts are parsed.
  fact-function)

(defn remove-namespace-facts-from! [namespace]
  (swap! global remove-namespace-facts-from namespace))

(defn remove-from! [namespace]
  (swap! global remove-from namespace))

(defn all-facts<> [] (all-facts @global))
(defn namespace-facts<> [namespace] (namespace-facts @global namespace))
(defn last-fact-checked<> [] (:last-fact-checked @global))
