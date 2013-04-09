(ns midje.Bootstrap)

(defonce bootstrapped false)

(defn bootstrap []
  (when-not bootstrapped
    (require 'midje.config)
    (let [saved-ns (ns-name *ns*)]
      (try
        (in-ns 'midje.config)
        ((ns-resolve 'midje.config 'load-config-files))
      (finally
        (in-ns saved-ns))))
        
    (require 'midje.emission.api)
    ( (ns-resolve 'midje.emission.api 'load-plugin)
      ( (ns-resolve 'midje.config 'choice) :emitter))

    ((ns-resolve 'midje.emission.colorize 'init!))

    ((ns-resolve 'midje.emission.state 'no-more-plugin-installation))
    (alter-var-root #'bootstrapped (constantly true))))

