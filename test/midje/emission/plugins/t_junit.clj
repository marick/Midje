(ns midje.emission.plugins.t-junit
  (:require [midje.sweet :refer :all]
            [midje.util :refer :all]
            [midje.test-util :refer :all]
            [midje.emission.plugins.junit :as plugin]
            [midje.config :as config]
            [midje.emission.plugins.default-failure-lines :as failure-lines]
            [midje.util.ecosystem :as ecosystem]))

(def stashed-config (identity config/*config*))
(println stashed-config)
(flush)

(defn innocuously [key & args]
  (config/with-augmented-config {:emitter     'midje.emission.plugins.junit
                                 :print-level :print-facts}
    (captured-output (apply (key plugin/emission-map) args))))

(def test-fact
  (with-meta (fn[]) {:midje/name "named" :midje/description "desc" :midje/namespace "blah"}))

(def test-failure-map
 {:type      :some-prerequisites-were-called-the-wrong-number-of-times,
  :namespace "midje.emission.plugins.t-junit"})

(fact "Entering a new namespace opens a file."
  (prerequisites
    (#'plugin/log-fn) => #(println %)
    (#'plugin/clear-file (contains "test-namespace")) => nil :times 1
    (#'plugin/clear-file (contains "other-namespace")) => nil :times 1
    (#'plugin/clear-file (contains "placeholder-to-reset-namespace")) => nil :times 1)

  (innocuously :possible-new-namespace 'test-namespace)
  => (contains "<testsuite name='test-namespace'")
  ;; re-entering namespace must not output anything
  (innocuously :possible-new-namespace 'test-namespace)
  => ""
  ;; entering a different namespace must close previous one
  (innocuously :possible-new-namespace 'other-namespace)
  ;; FIXME: nicer test for 'both strings present'
  => (contains (str "</testsuite>" "\n" "<testsuite name='other-namespace'"))
  (innocuously :possible-new-namespace 'placeholder-to-reset-namespace))

;; FIXME: this statefullnes is ugly and hard to test.
;; if this fact runs standalone, the :possible-new-namespace will try to clear
;; the report file. Otherwise, it was already done in previous test.
;; perhaps a plugin/clear-state in a fixture would help.
(fact "Closing a fact stream closes testsuite"
  (prerequisites
    (#'plugin/clear-file (contains "test-namespace")) => nil
    (#'plugin/clear-file (contains "placeholder-to-reset-namespace")) => nil :times 1
    (#'plugin/log-fn) => #(println %))

  (innocuously :possible-new-namespace 'test-namespace)
  (innocuously :finishing-fact-stream {} {})
  => (contains "</testsuite>")
  (innocuously :possible-new-namespace 'placeholder-to-reset-namespace))

(fact "pass produces a <testcase> tag"
  (plugin/starting-to-check-fact test-fact)
  (plugin/finishing-fact test-fact)

  (innocuously :pass) => (contains "<testcase ")
  (provided
    (#'plugin/log-fn) => #(println %)))

(fact "facts have an elapsed time"
  (plugin/starting-to-check-fact test-fact)
  (plugin/finishing-fact test-fact)

  (innocuously :pass) => (contains "time='")
  (provided
    (#'plugin/log-fn) => #(println %)))


(fact "failure produces a <testcase> tag"
  (plugin/starting-to-check-fact test-fact)

  (innocuously :fail test-failure-map) => (contains "<testcase ")
  (provided
    (#'plugin/log-fn) => #(println %)))

(fact "failure also produces a <failure> tag"
  (plugin/starting-to-check-fact test-fact)

  (innocuously :fail test-failure-map) => (contains "<failure type=':some-prerequisites-were-called-the-wrong-number-of-times'>")
  (provided
    (#'plugin/log-fn) => #(println %)))

;; TODO PLM: fix this horrible hack!
;; When the junit plugin loads, it manually resets the config. If you are
;; testing the junit plugin, this means that future tests in other files will
;; use that new config, which makes test output super verbose. The way forward,
;; as far as I can tell, is to somehow provide an unloading mechanism for midje
;; plugins, not unlike 'components'
(config/change-defaults :print-level :print-namespaces
                        :colorize (not (ecosystem/on-windows?)))
