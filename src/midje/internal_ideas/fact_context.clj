(ns ^{:doc "A way to keep track of the pertinent context of the current nested fact.
            Currently, that is only the fact's description/doc-string"}
  midje.internal-ideas.fact-context
  (:use [clojure.string :only [join]]))

(def ^{:dynamic true} *fact-context* [])

(defn nested-descriptions
  ([] *fact-context*)
  ([addition] (conj *fact-context* addition)))

(defmacro within-runtime-fact-context [description & body]
  `(binding [*fact-context* (conj *fact-context* ~description)]
     ~@body))

