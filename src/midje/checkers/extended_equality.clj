(ns midje.checkers.extended-equality
  (:require [midje.checking.core :as new-place]
            [midje.emission.deprecation :as deprecation]))

(defn extended-= [actual expected]
  (deprecation/deprecate "`extended-=` has moved to midje.checking.core. This version will be removed in 1.6.")
  (new-place/extended-= actual expected))
