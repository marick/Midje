(ns leiningen.midje
  (:require [leiningen.test :as lein]))

(defn midje [project]
  (lein/test project))
