(ns midje.unprocessed.background
)

(def *background-fakes* '())

(defn push-background-fakes [fakes]
  (alter-var-root #'*background-fakes* (partial cons fakes)))

(defn pop-background-fakes []
  (alter-var-root #'*background-fakes* rest))

(defn adding-background [fakes]
  (flatten (cons fakes *background-fakes*)))

(defmacro with-background-fakes [fakes & forms]
  `(try
     (push-background-fakes ~fakes)
     ~@forms
     (finally (pop-background-fakes))))
  
