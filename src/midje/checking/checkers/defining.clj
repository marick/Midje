(ns ^{:doc "Various ways to define checkers."}
  midje.checking.checkers.defining
  (:require [midje.util.pile :as pile]))

(defn as-checker
  "Label a function as a checker. This is only required if
   the checker is to be used on the left-hand-side of a prerequisite.
   See `(guide checkers-within-prerequisites)`.
   Example:

      (fact (f 3) => \"odd number received\"
        (provided
          (g (as-checker odd?)) => \"odd number received\"))
  "
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
    "Like defn, but tags the variable created and the function it
    refers to as checkers. This is only required if the checker is
    to be used in the left-hand side of a prerequisite, but it never
    hurts to define checkers using this. Here is a checker for
    positive even numbers:

        (defchecker twosie [actual]
           (and (pos? actual) (even? actual)))
        (fact 2 => twosie)

     Here is the definition of a simple version of the `roughly`
     checker:

       (defchecker roughly [expected delta]
          (checker [actual]
             (and (number? actual)
                  ...)))
      (fact 1.1 => (roughly 1 0.2))

     See also `(doc chatty-checker)`.
    "
    {:arglists '([name docstring? attr-map? bindings+bodies])}
    [name & args]
    (let [[docstring more-args] (pile/pop-docstring args)
          [attr-map bindings+bodies] (pile/pop-opts-map more-args)]
      (working-with-arglists+bodies name docstring attr-map bindings+bodies))))

(defmacro checker
  "Creates an anonymous function tagged as a checker. Such tagging is only
   needed if the checker is to be used on the left-hand-side of a
   prerequisite.  See `(guide checkers-within-prerequisites)`.
   Example:
       (provided
          (f (checker [x] (and (pos? x) (even? x)))) => 3)
  "
  [args & body]
  `(as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))

(def checker-makers '[defchecker checker as-checker])
