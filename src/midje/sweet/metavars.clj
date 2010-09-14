(ns midje.sweet.metavars
)

(defn- form-branch? [candidate]
  ;; In the absence of seqable?
  (or (seq? candidate)
      (map? candidate)
      (vector? candidate)
      (set? candidate)))

(defn metavar? [symbol-or-form]
  (and (symbol? symbol-or-form)
       (re-matches #"^\.+.+\.+" (name symbol-or-form))))

(defn define-metavars [form]
  (let [metavars (filter metavar? (tree-seq form-branch? seq form))]
    (doseq [metavar metavars]
      (intern *ns* metavar (symbol metavar)))
    metavars))

