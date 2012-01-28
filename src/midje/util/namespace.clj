(ns midje.util.namespace
  (:use midje.util.treelike
        [midje.util.thread-safe-var-nesting :only [var-root]])
  (:require [clojure.zip :as zip]))


(defmulti matches-symbols-in-semi-sweet-or-sweet-ns? (fn [symbols treelike] (tree-variant treelike)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :zipper [symbols loc]
   (matches-symbols-in-semi-sweet-or-sweet-ns? symbols (zip/node loc)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :form [symbols node]
  (let [base-names (map name symbols)
        qualified-names (concat (map #(str "midje.semi-sweet/" %) base-names)
                                (map #(str "midje.sweet/" %) base-names))]
    ( (set (concat base-names qualified-names)) (str node))))

(defn is-semi-sweet-keyword? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect fake not-called data-fake) loc))

(defn immigrate
  "Create a public var in this namespace for each public var in the
  namespaces named by ns-names. The created vars have the same name, root
  binding, and metadata as the original except that their :ns metadata
  value is this namespace."
  [& ns-names]
  (doseq [ns ns-names]
    (require ns)
    (doseq [[sym ^clojure.lang.Var var] (ns-publics ns)]
      (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
        (if (.hasRoot var)
          (intern *ns* sym (var-root var))
          (intern *ns* sym))))))