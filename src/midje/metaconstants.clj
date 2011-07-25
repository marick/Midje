;; -*- indent-tabs-mode: nil -*-

(ns midje.metaconstants
)

(defn- form-branch? [candidate]
  ;; In the absence of seqable?
  (or (seq? candidate)
      (map? candidate)
      (vector? candidate)
      (set? candidate)))

(defn metaconstant? [symbol-or-form]
  (and (symbol? symbol-or-form)
       (re-matches #"^\.+.+\.+" (name symbol-or-form))))

(defn define-metaconstants [form]
  (let [metaconstants (filter metaconstant? (tree-seq coll? seq form))]
    (doseq [metaconstant metaconstants]
      (intern *ns* metaconstant (symbol metaconstant)))
    metaconstants))

(def *metaconstant-counts*)

(defmacro with-fresh-generated-metaconstant-names [& forms]
  `(binding [*metaconstant-counts* (atom {})]
     ~@forms))

(defn metaconstant-for-form [[function-symbol & _ :as inner-form]]
  (let [swap-fn (fn [current-value function-symbol]
                  (if (current-value function-symbol)
                    (assoc current-value function-symbol
                           (inc (current-value function-symbol)))
                    (assoc current-value function-symbol 1)))
        number ((swap! *metaconstant-counts* swap-fn function-symbol)
                function-symbol)]
    (symbol (format "...%s-value-%s..." (name function-symbol) number))))

