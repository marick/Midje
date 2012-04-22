(ns midje.util.namespace
  (:use midje.util.treelike
        [midje.util.thread-safe-var-nesting :only [var-root]])
  (:require [clojure.zip :as zip]))


(defmulti matches-symbols-in-semi-sweet-or-sweet-ns? (fn [_symbols_ treelike] (tree-variant treelike)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :zipper [symbols loc]
   (matches-symbols-in-semi-sweet-or-sweet-ns? symbols (zip/node loc)))

(defmethod matches-symbols-in-semi-sweet-or-sweet-ns? :form [symbols node]
  (let [base-names       (map name symbols)
        semi-sweet-names (map #(str "midje.semi-sweet/" %) base-names)
        sweet-names      (map #(str "midje.sweet/" %) base-names)]
    (some #(= % (str node)) (concat base-names semi-sweet-names sweet-names))))

(defn semi-sweet-keyword? [loc]
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

(defn intern+keep-meta [ns sym v]
  (intern ns sym v)
  (let [newguy (get (ns-interns ns) sym)]
    (alter-meta! newguy merge (meta v))))