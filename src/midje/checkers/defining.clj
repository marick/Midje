;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Various ways to define checkers."}
  midje.checkers.defining
  (:use [midje.util.form-utils :only [pop-docstring pop-opts-map]]))

(defn as-checker
  "Turns an already existing function into a checker. Checkers can be used 
   to check fact results, as well as prerequisite calls."
  [function]
  (vary-meta function assoc :midje/checker true))

(letfn [(make-checker-definition [checker-name docstring attr-map arglists arglists+bodies]
          (let [metavars (merge {:midje/checker true :arglists `'~arglists}
                                (when docstring {:doc docstring})
                                 attr-map)
                name (vary-meta checker-name merge metavars)]
            `(def ~name (as-checker (fn ~checker-name ~@arglists+bodies)))))

        (working-with-arglists+bodies [checker-name docstring attr-map arglists+bodies]
          ;; Note: it's not strictly necessary to convert a single
          ;; body-and-arglist into a singleton list. However, that's what defn
          ;; does, and I thought it safer to be consistent.
          (if (vector? (first arglists+bodies))
            (recur checker-name docstring attr-map (list arglists+bodies))
            (let [arglists (map first arglists+bodies)]
              (make-checker-definition checker-name 
                                       docstring
                                       attr-map
                                       arglists
                                       arglists+bodies))))]

  (defmacro defchecker
    "Like defn, but tags the variable created and the function it refers to
     as checkers. Checkers can be used to check fact results, as well as prerequisite calls."
    {:arglists '([name docstring? attr-map? bindings+bodies])}
    [name & args]
    (let [[docstring more-args] (pop-docstring args)
          [attr-map bindings+bodies] (pop-opts-map more-args)]
      (working-with-arglists+bodies name docstring attr-map bindings+bodies))))

(defmacro checker
  "Creates an anonymous function tagged as a checker. Checkers can be used 
   to check fact results, as well as prerequisite calls."
  [args & body]
  `(as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))

(def checker-makers '[defchecker checker as-checker])
