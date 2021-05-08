(ns marick.midje
  (:require [clj-kondo.hooks-api :as hooks]
            [clojure.string :as string]))

(defn table-variable? [node]
  (let [sexpr (hooks/sexpr node)]
    (and (symbol? sexpr)
         (string/starts-with? (str sexpr) "?"))))

(defn ^:private let-with-table-variables [body bindings]
  (let [new-bindings (vec (reduce (fn [acc i]
                                    (concat acc [i nil])) [] bindings))]
    (hooks/list-node
     [(hooks/token-node 'let)
      (hooks/vector-node new-bindings)
      body])))

(defn tabular [{:keys [node]}]
  (let [[facts & bindings] (rest (:children node))
        new-node (->> bindings
                      (filter table-variable?)
                      (let-with-table-variables facts))]
    {:node new-node}))
