(ns ^{:doc "Customizable configuration"}
  midje.config
  (:use midje.clojure.core
        [midje.util.exceptions :only [user-error]])
  (:require [midje.emission.levels :as levels]
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


(defonce ^{:dynamic true}
  *config* {:print-level :print-normally
            :visible-deprecation true
            :visible-future true
            :partial-prerequisites false
            :check-after-creation true
            :emitter 'midje.emission.plugins.default
            :check-recorder (fn [example-map prerequisite-maps])
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

  
(defmacro with-augmented-config [additions & body]
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
   Does not affect any temporary (dynamic) configurations."
  [& kvs]
  (merge-permanently! (apply hash-map kvs)))

;; Levels

(defn- level-checker [operation]
  (fn [level-name]
    (operation (levels/normalize (choice :print-level))
               (levels/normalize level-name))))

(def at-or-above? (level-checker >=))
(def above?(level-checker >))

;; Fact functions

(defn user-wants-fact-to-be-recorded? [fact]
  ((choice :fact-filter) fact))

;; Convenience
(def no-overrides {})

(defn load-config-files []
  (dorun (map load-file ecosystem/config-files)))
