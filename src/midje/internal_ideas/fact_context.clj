(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def #^:private nested-description (atom []))

(defn- enter-context [description]
  (swap! nested-description conj description))

(defn- leave-context []
  (swap! nested-description #(vec (butlast %))))

(defmacro within-fact-context [description & body]
  `(try
     (#'enter-context ~description)
     ~@body
     (finally
       (#'leave-context))))

(defn nested-fact-description []
  (when-let [non-nil (seq (remove nil? @nested-description))]  
    (join " - " non-nil)))