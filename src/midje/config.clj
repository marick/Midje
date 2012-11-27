(ns ^{:doc "Customizable configuration"}
  midje.config
  (:refer-clojure :exclude [assoc!])
  (:use [midje.error-handling.exceptions :only [user-error]])
  (:require midje.ideas.reporting.level-defs
            [clojure.set :as set]))
  


(def ^{:dynamic true
       :doc "controls whether unmatched prerequisites 'fall through' to real function"}
     *allow-default-prerequisites* false)


(def ^{:dynamic true}
  *config* {:print-level :print-normally})

(defmulti validate-key! first)
(defmethod validate-key! :print-level [[_ value]]
  (midje.ideas.reporting.level-defs/validate-level! value))
(defmethod validate-key! :default [_])

(defn validate! [changes]
  (let [extras (set/difference (set (keys changes))
                               (set (keys *config*)))]
    (when (not (empty? extras))
      (throw (user-error (str "These are not configuration keys: " extras)))))
  (dorun (map validate-key! changes)))

  
(defmacro with-temporary-config [additions & body]
  `(let [true-map# ~additions]
     (validate! true-map#)
     (binding [*config* (merge *config* true-map#)]
       ~@body)))

(defn choice
  "Returns the configuration value of `key`"
  [key] (*config* key))
  
(defn merge! 
  "Merges the given map into the root configuration.
   Does not affect any temporary (dynamic) configurations."
  [additions]
  (validate! additions)
  (alter-var-root #'*config* merge additions))
  
(defn assoc! 
  "Adds key-value pairs to the root configuration.
   Does not affect any temporary (dynamic) configurations."
  [& kvs]
  (merge! (apply hash-map kvs)))

