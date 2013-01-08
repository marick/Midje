(ns midje.Bootstrap)

(defonce bootstrapped false)

(defn bootstrap []
  (when-not bootstrapped
    (require 'midje.config)
    
    (require 'midje.emission.api)
    ( (ns-resolve 'midje.emission.api 'load-plugin)
      ( (ns-resolve 'midje.config 'choice) :emitter))
    
    (alter-var-root #'bootstrapped (constantly true))))
