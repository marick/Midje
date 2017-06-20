(ns ^{:doc "Code used to swap out vars for faking prerequisites."}
  midje.util.thread-safe-var-nesting)

;; Variables that are expected to already be bound

(defn push-safely [the-var some-sequence]
  (alter-var-root the-var (partial cons some-sequence)))

(defn pop-safely [the-var]
  (alter-var-root the-var rest))


;; Variables that might not be bound
(def unbound-marker :midje/special-midje-unbound-marker)

(defn restore-one-root [[^clojure.lang.Var the-var new-value]]
  (if (= new-value unbound-marker)
    (.unbindRoot the-var)
    (alter-var-root the-var (fn [_current-value_] new-value))))

(defn alter-one-root [[^clojure.lang.Var the-var new-value]]
  (if (bound? the-var)
    (let [old-value (deref the-var)]
      (alter-var-root the-var (fn [_current-value_] new-value))
      [the-var old-value])
    (do
      (.bindRoot the-var new-value)
      [the-var unbound-marker])))

(defn with-altered-roots* [binding-map f]
  (let [old-bindings (into {} (for [var+new-value binding-map] (alter-one-root var+new-value)))]
    (try (f)
      (finally (dorun (map restore-one-root old-bindings))))))

(defmacro with-altered-roots
  "Used instead of with-bindings because bindings are thread-local
   and require specially declared vars."
  [binding-map & body]
  `(with-altered-roots* ~binding-map (fn [] ~@body)))


;; Values associated with namespaces
(defn set-namespace-value [key-name newval]
  (alter-meta! *ns* assoc key-name newval))

(defn destroy-namespace-value [key-name]
  (alter-meta! *ns* dissoc key-name))

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
  `(try
     (push-into-namespace ~key-name ~values)
     ~@forms
     (finally (pop-from-namespace ~key-name))))

(defn namespace-values-inside-out [key-name]
  (apply concat (namespace-value key-name)))
