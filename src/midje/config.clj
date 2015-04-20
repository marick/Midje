(ns ^{:doc "Customizable configuration"}
  midje.config
  (:use marick.clojure.core
        [midje.util.exceptions :only [user-error]])
  (:require [midje.emission.levels :as levels]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.pile :as pile]
            [midje.data.fact :as fact]))

  
;;; I consider whether we're running in the repl part of the config. This matters because
;;; threads can't examine the stack to see if they're in the repl. So we check once at
;;; startup time.

(def ^{:private true} started-in-repl?
     (try
       (throw (Exception.))
       (catch Exception e
         (not (empty? (filter #(.contains % "clojure.main$repl$read_eval_print")
                              (map str (.getStackTrace e))))))))

(defn running-in-repl? []
  started-in-repl?)



(defonce ^{:dynamic true}
  *config* {:print-level :print-normally
            :colorize (not (ecosystem/on-windows?))
            :visible-deprecation true
            :visible-future true
            :visible-failure-namespace false
            :partial-prerequisites false
            :check-after-creation true
            :emitter 'midje.emission.plugins.default
            :check-recorder (fn [checkable-map prerequisite-maps])
            :fact-filter (constantly true)})

(defmulti validate-key! first)
(defmethod validate-key! :print-level [[_ value]]
  (levels/validate-level! value))
(defmethod validate-key! :default [_])

(defn validate! [changes]
  (let [extras (difference (set (keys changes))
                               (set (keys *config*)))]
    (when (not (empty? extras))
      (throw (user-error (str "These are not configuration keys: " (vec extras))))))
  (dorun (map validate-key! changes)))

  
(defmacro with-augmented-config
  "Dynamically bind the configuration. Example:
   (require '[clojure.config :as config])
   (config/with-augmented-config {:check-after-creation false}
     (fact 1 => 2))"
  [additions & body]
  `(let [true-map# ~additions]
     (validate! true-map#)
     (binding [*config* (merge *config* true-map#)]
       ~@body)))

(defmacro at-print-level [level & body]
  `(with-augmented-config {:print-level ~level}
     ~@body))

(defn choice
  "Returns the configuration value of `key`"
  [key] (*config* key))

(defn merge-permanently! 
  "Merges the given map into the root configuration.
   Does not affect any temporary (dynamic) configurations."
  [additions]
  (validate! additions)
  (alter-var-root #'*config* merge additions))
  
(defn change-defaults
  "Adds key-value pairs to the root configuration.
   Does not affect any temporary (dynamic) configurations.

   Note: `change-defaults` can only be used in configuration files.
   It will not work within namespaces containing facts."
  [& kvs]
  (merge-permanently! (apply hash-map kvs)))

;; Levels

(defn- level-checker [operation]
  (fn [level-name]
    (operation (levels/normalize (choice :print-level))
               (levels/normalize level-name))))

(def at-or-above? (level-checker >=))
(def above?(level-checker >))

;; Fact filters

(def describes-name-matcher? stringlike?)
(defn describes-callable-matcher? [arg]
  (or (fn? arg) (keyword? arg)))

(defn name-matcher-for [desired]
  #(pile/stringlike-matches? desired (fact/name %)))
(defn callable-matcher-for [desired]
  (comp desired meta))

(defn appropriate-matcher-for [desired]
  ( (pred-cond desired
               describes-name-matcher? name-matcher-for
               describes-callable-matcher? callable-matcher-for
               :else (throw (Error. (str "Program error: Bad matcher for " desired))))
    desired))

(defn mkfn:fact-filter-predicate [desireds]
  (letfn [(make [fun source]
            (vary-meta fun assoc :created-from source))]
    (if (empty? desireds)
      (let [default-filter (choice :fact-filter)]
        (make (appropriate-matcher-for default-filter) [default-filter]))
      (make (pile/any-pred-from (map appropriate-matcher-for desireds)) desireds))))



(defn user-wants-fact-to-be-recorded? [fact]
  ((choice :fact-filter) fact))

;; Convenience
(def no-overrides {})

(defn load-config-files []
  (dorun (map load-file ecosystem/config-files)))

