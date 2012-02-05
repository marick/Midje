;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Macros for using protocols in prerequisites.

  The strategy for open protocols is to rewrite each function defined in the
  deftype/defrecord so that it checks whether its corresponding symbol is
  currently faked out. If so, it uses that function definition instead of
  continuing on with its own implementation."}
  midje.open-protocols
  (:use [midje.production-mode :only [user-desires-checking?]]
        [midje.internal-ideas.fakes :only [implements-a-fake?]]))

(defn- ^{:testable true } implementation?
  "Is this thing a protocol or a function definition?"
  [name-or-impl]
  (not (symbol? name-or-impl)))

(letfn [(open-spec
          ;;Return function that checks if its name has been bound to a fake-function.
          [[name args & body]]
          `(~name ~args
             (if (implements-a-fake? ~name)
               (apply ~name ~args)
               (do ~@body))))

        (revised-specs [specifications]
          (for [spec specifications]
            (if (and (user-desires-checking?) (implementation? spec))
              (open-spec spec)
              spec)))]

  (defmacro deftype-openly [name fields & specs]
    `(deftype ~name ~fields ~@(revised-specs specs)))

  (defmacro defrecord-openly [name fields & specs]
    `(defrecord ~name ~fields ~@(revised-specs specs))))

