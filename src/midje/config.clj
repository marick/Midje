(ns ^{:doc "Customizable configuration"}
  midje.config
  (:use [midje.error-handling.exceptions :only [user-error]])
  (:require midje.ideas.reporting.level-defs
            [clojure.set :as set]
            [midje.util.ecosystem :as ecosystem]))
  
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



;; TODO: In 1.6 or later, *allow-default-prerequisites* should be merged
;; into the *config* map. A deprecation notice has been added where it's used.
(def ^{:dynamic true
       :doc "Controls whether unmatched prerequisites 'fall through' to real function.
             DEPRECATED: Use config variable :partial-prerequisites instead."
       :deprecated "1.5"}
     *allow-default-prerequisites* false)


(def ^{:dynamic true}
  *config* {:print-level :print-normally
            :visible-deprecation true
            :visible-future true
            :partial-prerequisites false
            :check-after-creation true
            :emitter 'midje.emission.plugins.default

            ;; The following aren't changed by users.
            ;; Should they be in the fact-context?
            :desired-fact? (constantly true)})

(defmulti validate-key! first)
(defmethod validate-key! :print-level [[_ value]]
  (midje.ideas.reporting.level-defs/validate-level! value))
(defmethod validate-key! :default [_])

(defn validate! [changes]
  (let [extras (set/difference (set (keys changes))
                               (set (keys *config*)))]
    (when (not (empty? extras))
      (throw (user-error (str "These are not configuration keys: " (vec extras))))))
  (dorun (map validate-key! changes)))

  
(defmacro with-augmented-config [additions & body]
  `(let [true-map# ~additions]
     (validate! true-map#)
     (binding [*config* (merge *config* true-map#)]
       ~@body)))

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
   Does not affect any temporary (dynamic) configurations."
  [& kvs]
  (merge-permanently! (apply hash-map kvs)))

;; Convenience
(def no-overrides {})


;; This must be done here so that the code being loaded executes in
;; this namespace.
(dorun (map load-file ecosystem/config-files))

