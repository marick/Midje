(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def nested-descriptions (atom []))

(defn- enter-context [description]
  (swap! nested-descriptions conj description))

(defn- leave-context []
  (swap! nested-descriptions #(vec (butlast %))))

(defmacro within-fact-context [description & body]
  `(try
     (#'enter-context ~description)
     ~@body
     (finally
       (#'leave-context))))


;; A way to format the description - keeping formatting separate from representation.

; Used in the report

(defn format-nested-descriptions
  "Takes vector like [\"about cars\" nil \"sports cars are fast\"] and returns non-nils joined with -'s
   => \"about cars - sports cars are fast\""
  [nested-description-vector]
  (when-let [non-nil (seq (remove nil? nested-description-vector))]
    (join " - " non-nil)))