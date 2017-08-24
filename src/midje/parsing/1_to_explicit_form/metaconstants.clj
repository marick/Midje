(ns ^{:doc "Transforming code in a way that produces metaconstants"}
  midje.parsing.1-to-explicit-form.metaconstants
  (:require [midje.data.metaconstant :as data]))

(defn predefine-metaconstants-from-form [form]
  (let [metaconstant-symbols (set (filter data/metaconstant-symbol? (tree-seq coll? seq form)))]
    (doseq [symbol metaconstant-symbols]
      (intern *ns* symbol (data/metaconstant symbol {} nil)))
    metaconstant-symbols))

