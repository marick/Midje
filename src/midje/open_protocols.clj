(ns ^{:doc "Macros for using protocols in prerequisites.

  The strategy for open protocols is to rewrite each function defined in the
  deftype/defrecord so that it checks whether its corresponding symbol is
  currently faked out. If so, it uses that function definition instead of
  continuing on with its own implementation."}
  midje.open-protocols
  (:require [midje.data.prerequisite-state :refer [implements-a-fake?]]
            [midje.production-mode :refer [user-desires-checking?]]))

(defn- ^{:testable true } implementation?
  "Is this thing a protocol or a function definition?"
  [name-or-impl]
  (not (symbol? name-or-impl)))

(defn- ^:testable protocol?
  "Is this a java interface or class (like Object) or a Clojure protocol?"
  [symbol]
  (not (instance? java.lang.Class (resolve symbol))))

(defn- rewrite-as-mockable-function [[name args & body]]
  `(~name ~args
      (if (implements-a-fake? ~name)
        (apply ~name ~args)
        (do ~@body))))

(defn- rewrite-def*-body [body]
  (if (user-desires-checking?)
    (first
     (reduce (fn [[revised-body working-on-protocol?] form]
               (if (implementation? form)
                 (vector (conj revised-body (if working-on-protocol? (rewrite-as-mockable-function form) form))
                         working-on-protocol?)
                 (vector (conj revised-body form)
                         (protocol? form))))
             [[] false]
             body))
    body))

(defmacro deftype-openly [name fields & specs]
  `(deftype ~name ~fields ~@(rewrite-def*-body specs)))

(defmacro defrecord-openly [name fields & specs]
  `(defrecord ~name ~fields ~@(rewrite-def*-body specs)))

