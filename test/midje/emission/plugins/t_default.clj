(ns midje.emission.plugins.t-default
  (:use [midje sweet util test-util])
  (:require [midje.config :as config]
            [midje.emission.plugins.default :as plugin]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]
            [midje.clojure-test-facade :as ctf]))


(defmacro innocuously [& body]
  `(state/with-emission-map plugin/emission-map
     (captured-output
      ~@body)))

(fact "passes produce no output"
  (innocuously (emit/pass)) => "")

(fact "report fact being entered"
  (let [name+desc-fact (with-meta (fn[])
                         {:midje/name "named" :midje/description "desc"})
        desc-fact (with-meta (fn[]) {:midje/description "desc"})
        unnamed (with-meta (fn[]) {:midje/file "file" :midje/line 3})]

    
    (fact "prints names in preference to descriptions"
      (innocuously (plugin/starting-to-check-fact name+desc-fact)) => #"Checking named"
      (innocuously (plugin/starting-to-check-fact desc-fact)) => #"Checking desc"
      (innocuously (plugin/starting-to-check-fact unnamed)) => #"Checking fact at \(file:3\)")))

