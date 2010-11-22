(ns midje.util.thread-safe-var-nesting)

;; Variables that are expected to already be bound
(defn push-safely [the-var some-sequence]
  (alter-var-root the-var (partial cons some-sequence)))

(defn pop-safely [the-var]
  (alter-var-root the-var rest))


;; Variables that might not be bound
(def unbound-marker :midje/special-midje-unbound-marker)

(defn restore-one-root [[variable new-value]]
  (if (= new-value unbound-marker)
    (.unbindRoot variable)
    (alter-var-root variable (fn [current-value] new-value))))

(defn alter-one-root [[variable new-value]]
   (if (bound? variable) 
     (let [old-value (deref variable)]
       (alter-var-root variable (fn [current-value] new-value))
       [variable old-value])
     (do
       (.bindRoot variable new-value)
       [variable unbound-marker])))

(defn with-altered-roots* [binding-map function]
  (let [old-bindings (into {} (for [pair binding-map] (alter-one-root pair)))]
    (try (function)
	 ;; Can't use doseq inside a finally clause.
	 (finally (dorun (map restore-one-root old-bindings))))))

(defmacro with-altered-roots
  "Used instead of with-bindings because bindings are thread-local
   and will require specially declared vars in Clojure 1.3"
  [binding-map & rest]
  `(with-altered-roots* ~binding-map (fn [] ~@rest)))


;; "Variables" associated with namespaces
(defn set-namespace-pseudovariable [key-name newval] 
  (alter-meta! *ns* merge {key-name newval}))
