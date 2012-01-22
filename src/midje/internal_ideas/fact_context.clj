(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def #^:private *nested-descriptions* (atom []))

(defmacro within-fact-context [description & body]
  `(letfn [(enter-context# [description#]
             (swap! *nested-descriptions* conj description#))

           (leave-context# []
             (swap! *nested-descriptions* #(vec (butlast %))))]
     (try
       (enter-context# ~description)
       ~@body
       (finally
         (leave-context#)))))

(defn nested-fact-description []
  (when-let [non-nil (seq (remove nil? @*nested-descriptions*))]  
    (join " - " non-nil)))