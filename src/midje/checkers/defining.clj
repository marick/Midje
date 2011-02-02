;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers.defining)

(defn tag-as-checker [function]
  (vary-meta function merge {:midje/checker true}))


(defn- make-checker-definition [checker-name docstring args body]
;  (prn checker-name docstring args body)
  (let [core-function `(fn ~(vec args) ~@body)
        core-metavars {:midje/checker true :arglists `'~(list args)}
        metavars-to-add (if docstring
                          (assoc core-metavars :doc docstring)
                          core-metavars)]
    `(def ~(vary-meta checker-name merge metavars-to-add)
          (.withMeta ~core-function
                     (assoc (.meta (var ~checker-name)) :midje/checker true)))))

(defn- working-with-body-and-args [checker-name docstring body-and-args]
;  (prn checker-name docstring body-and-args)
  (make-checker-definition checker-name docstring
                           (first body-and-args) (rest body-and-args)))
  

(defmacro defchecker [checker-name & stuff]
  "Like defn, but tags the variable created and the function it refers to
   as checkers."

;  (prn "XXXX" [checker-name stuff])
  (if (string? (first stuff))
    (working-with-body-and-args checker-name (first stuff) (rest stuff))
    (working-with-body-and-args checker-name nil stuff)))
    
(defmacro checker [args & body]
  `(tag-as-checker (fn ~(vec args) ~@body)))

(defn checker? [item]
  (:midje/checker (meta item)))
  
