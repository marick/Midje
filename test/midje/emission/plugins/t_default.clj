(ns midje.emission.plugins.t-default
  (:use [midje sweet util test-util])
  (:require [midje.emission.plugins.default :as plugin]
            [midje.emission.state :as state]))

(defn innocuously [key & args]
  (captured-output (apply (key plugin/emission-map) args)))

(fact "passes produce no output"
  (innocuously :pass) => "")

(fact "report fact being entered"
  (let [name+desc-fact (with-meta (fn[])
                         {:midje/name "named" :midje/description "desc"})
        desc-fact (with-meta (fn[]) {:midje/description "desc"})
        unnamed (with-meta (fn[]) {:midje/file "file" :midje/line 3})]

    
    (fact "prints names in preference to descriptions"
      (innocuously :starting-to-check-fact name+desc-fact) => #"Checking named"
      (innocuously :starting-to-check-fact desc-fact) => #"Checking desc"
      (innocuously :starting-to-check-fact unnamed) => #"Checking fact at \(file:3\)")))

(fact "reports only when namespace changes"
   (plugin/set-last-namespace-shown! 'nothing)
   (innocuously :possible-new-namespace 'nothing) => ""
   (innocuously :possible-new-namespace 'something) => #"something"
   (innocuously :possible-new-namespace 'something) => "")

