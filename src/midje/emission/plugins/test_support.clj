(ns ^{:doc "An emission map whose functions can be mocked."}
  midje.emission.plugins.test-support
  (:require [midje.emission.plugins.silence :as silence]))

(def pass silence/ignore)
(def fail silence/ignore)
(def forget-everything silence/ignore)

(def emission-map {:pass #'pass
                   :fail #'fail
                   :forget-everything #'forget-everything
                   })

