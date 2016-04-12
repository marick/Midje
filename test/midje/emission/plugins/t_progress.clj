(ns midje.emission.plugins.t-progress
  (:require [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.emission.plugins.progress :as plugin]
            [midje.config :as config]
            [midje.emission.colorize :as color]
            [midje.emission.state :as state]))

(defn innocuously [key & args]
  (config/with-augmented-config {:emitter 'midje.emission.plugins.progress
                                 :print-level :print-facts}
    (captured-output (apply (key plugin/emission-map) args))))

(fact "pass produces a ."
  (innocuously :pass) => (color/pass "."))

(fact "failure produces a F"
  (innocuously :fail ..failure-map..) => (color/fail "F"))

(fact "future fact produces a P"
  (innocuously :future-fact ..description.. ..position..) => (color/note "P"))

(fact "produces a summary of all errors encountered"
  (state/add-raw-fact-failure! {:type :actual-result-did-not-match-expected-value :expected-result 1 :actual 2 :position ["blah.clj" 100]})

  (innocuously :starting-fact-stream)
  (innocuously :finishing-fact-stream {:midje-failures 1 :midje-passes 1} {:test 0}) => #"blah.clj:100")

(fact "produces a summary from state information"
  (let [minimal-midje {:midje-passes 0
                       :midje-failures 0}
        minimal-ct {:test 0}]

    (innocuously :starting-fact-stream)
    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-failures 3 :midje-passes 2) minimal-ct)
    => #"FAILURE.*3 checks failed.*But 2 succeeded"))
