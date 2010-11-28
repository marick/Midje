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


;; Values associated with namespaces
(defn set-namespace-value [key-name newval] 
  (alter-meta! *ns* merge {key-name newval}))

(defn namespace-value [key-name]
  (key-name (meta *ns*)))

(defn push-into-namespace [key-name newvals]
;  (println "push onto" key-name "these" newvals)
  (set-namespace-value key-name (cons (reverse newvals)
				      (namespace-value key-name))))

(defn pop-from-namespace [key-name]
;  (println "popping" key-name)
  (set-namespace-value key-name (rest (namespace-value key-name))))

(defmacro with-pushed-namespace-values [key-name values & forms]
  ;; (println "== with-pushed-namespace-values")
  ;; (println "== " key-name)
  ;; (println "== " values)
  ;; (println "== " forms)
  `(try
     (push-into-namespace ~key-name ~values)
     ~@forms
     (finally (pop-from-namespace ~key-name))))

(defn namespace-values-inside-out [key-name]
  (apply concat (namespace-value key-name)))
