(ns ^{:doc "Support for running and rerunning facts"}
  midje.ideas.rerunning-facts
  (:use [midje.ideas.metadata :only [separate-metadata
                                     fact-name fact-true-name
                                     fact-source fact-namespace]]
        clojure.pprint
        [midje.util.form-utils :only [dissoc-keypath]])
  (:require [midje.internal-ideas.compendium :as compendium]))

;;; Note: This code is tested indirectly, via t_sweet.clj and t_repl.clj.

(def compendium (atom (compendium/fresh-compendium)))

;;; Macroexpansion-time support functions

(def ^{:dynamic true} *parse-time-fact-level* 0)

(defn- working-on-top-level-fact? []
  (= *parse-time-fact-level* 1))
  
(defmacro given-possible-fact-nesting [& forms]
  `(binding [*parse-time-fact-level* (inc *parse-time-fact-level*)]
     ~@forms))

(defmacro working-on-nested-facts [& forms]
  ;; Make sure we don't treat this as a top-level fact
  `(binding [*parse-time-fact-level* (+ 2 *parse-time-fact-level*)]
     ~@forms))

(defn wrap-with-check-time-fact-recording [form this-function-here-symbol]
  (if (working-on-top-level-fact?)
    `(do (record-fact-check (~this-function-here-symbol))
         ~form)
    form))

;; The rather hackish construction here is to keep
;; the expanded fact body out of square brackets because
;; `tabular` expansions use `seq-zip`. 

(defn wrap-with-creation-time-fact-recording [function-form]
  (if (working-on-top-level-fact?)
    `((fn [fact-function#]
        (record-fact-existence fact-function#)
        fact-function#)
      ~function-form)
    function-form))



;;; Operations on the mutable compendium

(defn last-fact-function-run []
  (:last-fact-checked @compendium))

(defn record-fact-check [function]
  (when-not (:check-only-at-load-time (meta function))
    (swap! compendium assoc :last-fact-checked function)))


(defn forget-facts-in-namespace [namespace]
  (swap! compendium compendium/remove-namespace-facts-from namespace))

(defn reset-compendium []
  (reset! compendium (compendium/fresh-compendium)))

(defn compendium-contents []
  (compendium/all-facts @compendium))
  
(defn namespace-facts [namespace]
  (compendium/namespace-facts @compendium namespace))

(defn record-fact-existence [fact-function]
  (when-not (:check-only-at-load-time (meta fact-function))
    (if-let [previous (compendium/previous-version @compendium fact-function)]
      (swap! compendium compendium/remove-from previous))
    (swap! compendium compendium/add-to fact-function)))

;;; Running facts
  
(defn check-some-facts [fact-functions]
  (every? true? (doall (map #(%) fact-functions))))

