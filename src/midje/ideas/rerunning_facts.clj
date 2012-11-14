(ns ^{:doc "Support for loading, running, and rerunning facts"}
  midje.ideas.rerunning-facts
  (:use [midje.ideas.metadata :only [separate-metadata
                                     fact-name fact-true-name
                                     fact-source fact-namespace]]
        [midje.util.form-utils :only [dissoc-keypath]])
  (:require [midje.internal-ideas.compendium :as compendium]))

;;; Where storage happens

;; Note: The history is a symbol that, when looked up in
;; the `fact-var-namespace`, yields pointer to a fact-function.
(def fact-check-history (atom (constantly true)))

(def compendium (atom (compendium/fresh-compendium)))

;;; Macroexpansion-time support functions

(def ^{:dynamic true} *parse-time-fact-level* 0)

(defmacro given-possible-fact-nesting [& forms]
  `(binding [*parse-time-fact-level* (inc *parse-time-fact-level*)]
     ~@forms))

(defmacro working-on-nested-facts [& forms]
  ;; Make sure we don't treat this as a top-level fact
  `(binding [*parse-time-fact-level* (+ 2 *parse-time-fact-level*)]
     ~@forms))

(defn wrap-with-check-time-fact-recording [true-name form]
  (if (= *parse-time-fact-level* 1)
    `(do (record-fact-check '~true-name)
         ~form)
    form))


;;; Runtime support of a history of which facts run

(defn last-fact-function-run
  [] 
  @(ns-resolve compendium/fact-var-namespace @fact-check-history))

(defn record-fact-check [true-name]
  (reset! fact-check-history true-name))



;;; Operations on the mutable compendium

(defn forget-facts-in-namespace [namespace]
  (swap! compendium compendium/remove-namespace-facts-from namespace))

(defn reset-compendium []
  (reset! compendium (compendium/fresh-compendium)))

(defn compendium-contents []
  (compendium/all-facts @compendium))
  
(defn namespace-facts [namespace]
  (compendium/namespace-facts @compendium namespace))

(defn record-fact-existence [fact-function]
  (if-let [previous (compendium/previous-version @compendium fact-function)]
    (swap! compendium compendium/remove-from previous))
  (swap! compendium compendium/add-to fact-function))

;;; Running facts
  
(defn check-some-facts [fact-functions]
  (every? true? (doall (map #(%) fact-functions))))

