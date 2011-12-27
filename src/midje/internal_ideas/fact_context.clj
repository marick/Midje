(ns midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def ^{:private true} nested-descriptions (atom []))

(defn- enter-context [description]
  (swap! nested-descriptions conj description))

(defn- leave-context []
  (swap! nested-descriptions #(vec (butlast %))))

(defmacro within-fact-context [description & body]
  `(do
     (#'enter-context ~description)
     ~@body
     (#'leave-context)))

(defn nested-fact-description []
  (when-let [non-nil (seq (remove nil? @nested-descriptions))]  
    (join " - " non-nil)))