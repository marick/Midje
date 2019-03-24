(ns ^{:doc "Functions dealing with making various forms of
            Midje output be ergonomically colorful."}
  midje.emission.colorize
  (:require [colorize.core :as color]
            [midje.config :as config]))

;; This indirection is required so that the tests of this
;; file can fake the prerequisite
(def config-choice config/choice)

(defmacro ^:private def-colorize
  [fn-name on reverse]
  `(defn ~fn-name
     [s#]
     ((case (config-choice :colorize)
        :true ~on
        :reverse ~reverse
        str)
      s#)))

(def-colorize fail
  color/red color/red-bg)

(def-colorize pass
  color/green color/green-bg)

(def-colorize note
  color/cyan color/cyan-bg)
