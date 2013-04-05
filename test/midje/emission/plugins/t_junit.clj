(ns midje.emission.plugins.t-junit
  (:use [midje sweet util test-util])
  (:require [midje.emission.plugins.junit :as plugin]
            [midje.config :as config]
            [midje.emission.plugins.default-failure-lines :as failure-lines]))

(defn innocuously [key & args]
  (config/with-augmented-config {:emitter 'midje.emission.plugins.junit
                                 :print-level :print-facts}
    (captured-output (apply (key plugin/emission-map) args))))

(def test-fact
  (with-meta (fn[]) {:midje/name "named" :midje/description "desc" :midje/namespace "blah"}))

(def test-failure-map
 {:type :some-prerequisites-were-called-the-wrong-number-of-times,
   :namespace "midje.emission.plugins.t-junit"})

(fact "starting a fact stream opens a <testsuite>"
  (innocuously :starting-fact-stream) => (contains "<testsuite>")
  (provided
    (plugin/log-fn) => #(println %)))

(fact "closing a fact stream closes </testsuite>"
  (plugin/def-fact-cache)

  (innocuously :finishing-fact-stream {} {}) => (contains "</testsuite>")
  (provided
    (plugin/log-fn) => #(println %)))

(fact "pass produces a <testcase> tag"
  (plugin/def-fact-cache)
  (plugin/starting-to-check-fact test-fact)

  (innocuously :pass) => (contains "<testcase classname='blah' name='named'/>")
  (provided
    (plugin/log-fn) => #(println %)))

(fact "failure produces a <testcase> tag"
  (plugin/def-fact-cache)
  (plugin/starting-to-check-fact test-fact)

  (innocuously :fail test-failure-map) => (contains "<testcase classname='blah' name='named'>")
  (provided
    (plugin/log-fn) => #(println %)))

(fact "failure also produces a <failure> tag"
  (plugin/def-fact-cache)
  (plugin/starting-to-check-fact test-fact)

  (innocuously :fail test-failure-map) => (contains "<failure type=':some-prerequisites-were-called-the-wrong-number-of-times'>")
  (provided
    (plugin/log-fn) => #(println %)))
