(ns midje.bootstrap
  (:require [midje.config :as config]
            [midje.emission.api :as emission.api]
            [midje.emission.colorize :as emission.colorize]
            [midje.emission.state :as emission.state]))

(defonce bootstrapped false)

(defn bootstrap []
  (when-not bootstrapped

    (let [saved-ns (ns-name *ns*)]
      (try
        (in-ns 'midje.config)
        ((ns-resolve 'midje.config 'load-config-files))
      (finally
        (in-ns saved-ns))))

    (emission.api/load-plugin (config/choice :emitter))
    (emission.colorize/init!)
    (emission.state/no-more-plugin-installation)

    (alter-var-root #'bootstrapped (constantly true))))
