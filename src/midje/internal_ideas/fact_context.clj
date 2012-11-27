(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def nested-descriptions (atom []))

(defn- enter-runtime-context [description]
  (swap! nested-descriptions conj description))

(defn- leave-runtime-context []
  (swap! nested-descriptions #(vec (butlast %))))

(defmacro within-runtime-fact-context [description & body]
  `(try
     (#'enter-runtime-context ~description)
     ~@body
     (finally
       (#'leave-runtime-context))))


