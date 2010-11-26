(ns midje.util.form-utils)

;; TODO: had to change list? to sequential? because unification produces lazyseqs.
(defn form-first? [form desired]
  (and (sequential? form)
       (symbol? (first form))
       (= (name (first form)) desired)))

