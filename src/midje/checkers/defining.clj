;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.defining)

(defn as-checker [function]
  (vary-meta function merge {:midje/checker true}))


(defn- make-checker-definition [checker-name docstring arglists bodies-and-arglists]
  (let [core-function `(fn ~checker-name ~@bodies-and-arglists)
        core-metavars {:midje/checker true :arglists `'~arglists}
        metavars-to-add (if docstring
                          (assoc core-metavars :doc docstring)
                          core-metavars)]
    `(def ~(vary-meta checker-name merge metavars-to-add)
          (.withMeta ~core-function
                     (assoc (.meta (var ~checker-name)) :midje/checker true)))))


(defn- working-with-bodies-and-arglists [checker-name docstring bodies-and-arglists]
                                        ;  (prn checker-name docstring bodies-and-arglists)
  ;; Note: it's not strictly necessary to convert a single
  ;; body-and-arglist into a singleton list. However, that's what defn
  ;; does, and I thought it safer to be consistent.
  (if (vector? (first bodies-and-arglists))
    (recur checker-name docstring (list bodies-and-arglists))
    (let [arglist-finder #(map first %)]
      (make-checker-definition checker-name docstring
                               (arglist-finder bodies-and-arglists)
                               bodies-and-arglists))))

(defmacro defchecker [checker-name & stuff]
  "Like defn, but tags the variable created and the function it refers to
   as checkers."
  (if (string? (first stuff))
    (working-with-bodies-and-arglists checker-name (first stuff) (rest stuff))
    (working-with-bodies-and-arglists checker-name nil stuff)))
    
(defmacro checker [args & body]
  `(as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))

(def checker-makers '[defchecker checker as-checker])
