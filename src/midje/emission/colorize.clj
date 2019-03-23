(ns ^{:doc "Functions dealing with making various forms of
            Midje output be ergonomically colorful."}
  midje.emission.colorize
  (:require [colorize.core :as color]
            [clojure.string :as str]
            [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]))

;; This indirection is required so that the tests of this
;; file can fake the prerequisite
(def config-choice config/choice)

(defn- config-choice-as-string []
  (let [choice (config-choice :colorize)]
    (if (keyword? choice)
      (name choice)
      (str choice))))

(defn- colorize-string []
  (str/upper-case (or (ecosystem/getenv "MIDJE_COLORIZE")
                      (config-choice-as-string))))

(defmacro ^:private def-colorize
  [fn-name on reverse]
  `(defn ~fn-name
     [s#]
     ((case (colorize-string)
        "TRUE" ~on
        "REVERSE" ~reverse
        str)
      s#)))

(def-colorize fail
  color/red color/red-bg)

(def-colorize pass
  color/green color/green-bg)

(def-colorize note
  color/cyan color/cyan-bg)
