(ns midje.sweet.metaconstants
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

