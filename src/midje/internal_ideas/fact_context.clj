(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def fact-context (atom []))

(defn nested-descriptions []
  @fact-context)

(defn- enter-runtime-context [description]
  (swap! fact-context conj description))

(defn- leave-runtime-context []
  (swap! fact-context #(vec (butlast %))))

(defmacro within-runtime-fact-context [description & body]
  `(try
     (#'enter-runtime-context ~description)
     ~@body
     (finally
       (#'leave-runtime-context))))


