(ns ^{:doc "Transforming code in a way that produces metaconstants"}
  midje.parsing.1-to-explicit-form.metaconstants
  (:require [midje.data.metaconstant :as data])
  (:import midje.data.metaconstant.Metaconstant))

(defn predefine-metaconstants-from-form [form]
  (let [metaconstant-symbols (set (filter data/metaconstant-symbol? (tree-seq coll? seq form)))]
    (doseq [symbol metaconstant-symbols]
      (intern *ns* symbol (Metaconstant. symbol {} nil)))
    metaconstant-symbols))

