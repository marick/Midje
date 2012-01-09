;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.defining)

(defn as-checker [function]
  (vary-meta function merge {:midje/checker true}))

(letfn [(make-checker-definition [checker-name docstring attr-map arglists arglists+bodies]
          (let [metavars (merge {:midje/checker true :arglists `'~arglists}
                                (when docstring {:doc docstring})
                                 attr-map)
                name (vary-meta checker-name merge metavars)
                checker-fn `(as-checker (fn ~checker-name ~@arglists+bodies))]
            `(def ~name ~checker-fn)))

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
     as checkers."
    [checker-name & stuff]
    (cond 
      (and (string? (first stuff)) (map? (second stuff)))
      (working-with-arglists+bodies checker-name (first stuff) (second stuff) (drop 2 stuff))
    
      (map? (first stuff))
      (working-with-arglists+bodies checker-name nil (first stuff) (rest stuff))
    
      (string? (first stuff))
      (working-with-arglists+bodies checker-name (first stuff) {} (rest stuff))
    
      :else
      (working-with-arglists+bodies checker-name nil {} stuff))))

(defmacro checker [args & body]
  `(as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))

(def checker-makers '[defchecker checker as-checker])
