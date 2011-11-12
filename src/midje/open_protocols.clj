;; -*- indent-tabs-mode: nil -*-

(ns midje.open-protocols
  (:use [midje.production-mode :only [user-desires-checking?]]
        [midje.internal-ideas.fakes :only [implements-a-fake?]]))

;;; The strategy for open protocols is to rewrite each function defined in the
;;; deftype/defrecord so that it checks whether its corresponding symbol is
;;; currently faked out. If so, it uses that function definition instead of
;;; continuing on with its own implementation.

(defn- implementation? [name-or-impl]
  "Is this thing a protocol or a function definition?"
  (not (symbol? name-or-impl)))

(defn- open-spec [[name args & body]]
  "Return function that checks if its name has been bound to a fake-function."
  `(~name ~args
     (if (implements-a-fake? ~name)
       (apply ~name ~args)
       (do ~@body))))

(defn- defx-openly [x name fields specs]
  (let [revised-specs (for [spec specs]
                        (if (and (implementation? spec)
                              (user-desires-checking?))
                          (open-spec spec)
                          spec))]
    (list* x name fields revised-specs)))

(defmacro deftype-openly [name fields & specs]
  (defx-openly 'deftype name fields specs))
(defmacro defrecord-openly [name fields & specs]
  (defx-openly 'defrecord name fields specs))

