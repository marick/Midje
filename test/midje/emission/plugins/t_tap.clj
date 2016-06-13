(ns midje.emission.plugins.t-tap
  (:require [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.emission.plugins.tap :as tap]
            [midje.config :as config]
            [midje.emission.plugins.default-failure-lines :as failure-lines]
            [midje.emission.plugins.util :as util]))

(defn innocuously [key & args]
  (config/with-augmented-config {:emitter 'midje.emission.plugins.tap
                                 :print-level :print-facts}
    (captured-output (apply (key tap/emission-map) args))))

(def test-fact
  (with-meta (fn[]) {:midje/name "named" :midje/description "desc" :midje/namespace "blah"}))

(def test-failure-map
  {:type :some-prerequisites-were-called-the-wrong-number-of-times,
   :namespace "midje.emission.plugins.t-tap"})

(fact
 "starting a fact stream resets fact-counter and last-namespace-shown"
 (innocuously :starting-fact-stream) => ""
 @tap/fact-counter => 0
 @tap/last-namespace-shown => nil)

(fact
 "starting to check fact emits a message"
 (innocuously :starting-to-check-fact test-fact) => "# Checking named\n")

(fact
 "closing a fact stream generates the closing report"
 (innocuously :finishing-fact-stream
              {:midje-passes 1 :midje-failures 1} ..ignored..)
 => (contains "1..0 # midje count: 2")
 (fact
  "with empty midje-counters set to 0"
  (innocuously :finishing-fact-stream
               {:midje-passes 0 :midje-failures 0}
               ..ignored..)
  => (contains "# No facts were checked. Is that what you wanted?"))
 (fact
  "with empty midje-counters"
  (innocuously :finishing-fact-stream {} ..ignored..)
  => (contains "# No facts were checked. Is that what you wanted?")))

(with-state-changes [(before :facts (do (reset! tap/fact-counter 0)
                                        (tap/set-last-namespace-shown! nil)))]
  (fact
   "pass produces ok with the index of the test"
   (innocuously :pass) => (contains "ok 1\n")
   (innocuously :pass) => (contains "ok 2\n")))

(with-state-changes [(before :facts (do (reset! tap/fact-counter 0)
                                        (tap/set-last-namespace-shown! nil)))]
  (fact
   "failure produces not ok with the index of the test"
   (innocuously :fail test-failure-map) => (contains "\nnot ok 1")
   (innocuously :fail test-failure-map) => (contains "\nnot ok 2")))


(with-state-changes [(before :facts (do (reset! tap/fact-counter 0)
                                        (tap/set-last-namespace-shown! nil)))]
  (fact "failure also produces not ok"
        (innocuously :fail test-failure-map) => (contains "\nnot ok 1")))
