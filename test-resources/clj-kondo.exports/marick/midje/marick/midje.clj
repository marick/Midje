(ns marick.midje
  (:require [clj-kondo.hooks-api :as hooks]
            [clojure.string :as string]))

(defn table-variable? [node]
  (let [sexpr (hooks/sexpr node)]
    (and (symbol? sexpr)
         (string/starts-with? (str sexpr) "?"))))

(defn ^:private let-with-table-variables [body bindings]
  (let [new-bindings (vec (reduce (fn [acc i] (concat acc [i (hooks/token-node 'identity)]))
                                  [] bindings))]
    (hooks/list-node
     [(hooks/token-node 'let)
      (hooks/vector-node new-bindings)
      body])))

(defn ^:private do-form [forms]
  (hooks/list-node
    (concat [(hooks/token-node 'do)]
            forms)))

(defn tabular [{:keys [node]}]
  (let [[facts vec-bindings & bindings] (rest (:children node))]
    (if (hooks/vector-node? vec-bindings)
      {:node (let-with-table-variables
               (do-form (cons facts bindings))
               (map hooks/token-node (hooks/sexpr vec-bindings)))}
      {:node (->> (cons vec-bindings bindings)
                  (filter table-variable?)
                  (let-with-table-variables facts))})))
