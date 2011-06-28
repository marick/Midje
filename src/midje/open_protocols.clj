;; -*- indent-tabs-mode: nil -*-

(ns midje.open-protocols
  (:use [midje.production-mode :only [user-desires-checking?]]))

(defn- implementation? [name-or-impl]
  (not (symbol? name-or-impl)))

(defn- open-spec [[name args & body]]
  `(~name ~args
     (if (midje.fakes/function-tagged-as-fake? ~name)
       (apply ~name ~args)
       (do ~@body))))

(defn- defx-openly [x name fields specs]
  (let [revised-specs (map #(if (and (implementation? %)
                                     (user-desires-checking?))
                              (open-spec %)
                              %)
                           specs)]
    (list* x name fields revised-specs)))

(defmacro deftype-openly [name fields & specs]
  (defx-openly 'deftype name fields specs))
(defmacro defrecord-openly [name fields & specs]
  (defx-openly 'defrecord name fields specs))

