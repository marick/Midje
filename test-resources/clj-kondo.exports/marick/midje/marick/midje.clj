(ns marick.midje
  (:require [clj-kondo.hooks-api :as hooks]
            [clojure.string :as string]))

(def arrows '#{=>
               =not=>
               =deny=>
               =expands-to=>
               =future=>
               =contains=>
               =streams=>
               =throws=>
               =test=>
               =throw-parse-exception=>})

(defn ^:private index-of [e coll]
  (first (keep-indexed #(when (= e %2) %1) coll)))

(defn ^:private let-form [body bindings]
  (let [new-bindings (vec (reduce (fn [acc i]
                                    (concat acc [i (hooks/token-node 'identity)]))
                                  [] bindings))]
    (hooks/list-node
     [(hooks/token-node 'let)
      (hooks/vector-node new-bindings)
      body])))

(defn ^:private do-form [forms]
  (hooks/list-node
   (concat [(hooks/token-node 'do)]
           forms)))

(defn ^:private table-variable? [node]
  (let [sexpr (hooks/sexpr node)]
    (and (symbol? sexpr)
         (string/starts-with? (str sexpr) "?"))))

(defn ^:private handle-fact-outside-tabular [children arrow]
  (let [pos-arrow-index (inc (index-of arrow children))
        body (do-form children)
        bindings (->> children
                      (map-indexed (fn [i el]
                                     (when (> i pos-arrow-index)
                                       el)))
                      (remove nil?))]
    (if (hooks/vector-node? (first bindings))
      {:node (->> (hooks/sexpr (first bindings))
                  (map hooks/token-node)
                  (let-form body))}
      {:node (->> bindings
                  (filter table-variable?)
                  (let-form body))})))

(defn fact-tabular [fact vec-bindings bindings]
  (let [body (do-form (cons fact bindings))]
    (if (hooks/vector-node? vec-bindings)
      {:node (->> (hooks/sexpr vec-bindings)
                  (map hooks/token-node)
                  (let-form body))}
      {:node (->> (cons vec-bindings bindings)
                  (filter table-variable?)
                  (let-form body))})))

(defn ^:private handle-fact-inside-tabular [children]
  (if (hooks/string-node? (first children))
    (let [[_name fact vec-bindings & bindings] children]
      (fact-tabular fact vec-bindings bindings))
    (let [[fact vec-bindings & bindings] children]
      (fact-tabular fact vec-bindings bindings))))

(defn tabular [{:keys [node]}]
  (let [children (rest (:children node))
        fact-outside (first (filter #(contains? arrows (hooks/sexpr %)) children))]
    (if fact-outside
      (handle-fact-outside-tabular children fact-outside)
      (handle-fact-inside-tabular children))))
