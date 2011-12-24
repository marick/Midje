;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.defining)

(defn as-checker [function]
  (vary-meta function merge {:midje/checker true}))

(letfn [(make-checker-definition [checker-name docstring arglists bodies-and-arglists]
          (let [metavars (merge {:midje/checker true :arglists `'~arglists}
            (when docstring {:doc docstring}))
                name (vary-meta checker-name merge metavars)
                checker-fn `(as-checker (fn ~checker-name ~@bodies-and-arglists))]
            `(def ~name ~checker-fn)))

        (working-with-bodies-and-arglists [checker-name docstring arglists+bodies]
          ;  (prn checker-name docstring arglists+bodies)
          ;; Note: it's not strictly necessary to convert a single
          ;; body-and-arglist into a singleton list. However, that's what defn
          ;; does, and I thought it safer to be consistent.
          (if (vector? (first arglists+bodies))
            (recur checker-name docstring (list arglists+bodies))
            (let [arglist-finder #(map first %)]
              (make-checker-definition checker-name docstring
                (arglist-finder arglists+bodies)
                arglists+bodies))))]

  (defmacro defchecker
    "Like defn, but tags the variable created and the function it refers to
     as checkers."
    [checker-name & stuff]
    (if (string? (first stuff))
      (working-with-bodies-and-arglists checker-name (first stuff) (rest stuff))
      (working-with-bodies-and-arglists checker-name nil stuff))))

(defmacro checker [args & body]
  `(as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))

(def checker-makers '[defchecker checker as-checker])
