(ns midje.emission.plugins.t-default
  (:use midje.emission.plugins.default
        [midje sweet util test-util])
  (:require [midje.config :as config]
            [midje.clojure-test-facade :as ctf]))


(fact "report fact being entered"
  (let [name+desc-fact (with-meta (fn[])
                         {:midje/name "named" :midje/description "desc"})
        desc-fact (with-meta (fn[]) {:midje/description "desc"})
        unnamed (with-meta (fn[]) {:midje/file "file" :midje/line 3})]

    
    (fact "prints names in preference to descriptions"
      (captured-output (starting-to-check-fact name+desc-fact)) => #"Checking named"
      (captured-output (starting-to-check-fact desc-fact)) => #"Checking desc"
      (captured-output (starting-to-check-fact unnamed)) => #"Checking fact at \(file:3\)")))

