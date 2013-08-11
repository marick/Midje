(ns ^{:doc "JUnit formatter for Midje output"}
  midje.emission.plugins.junit
  (:use
    [midje.emission.util])
  (:require [midje.config :as config]
            [midje.data.fact :as fact]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as silence]
            [midje.emission.plugins.default-failure-lines :as lines]
            [clojure.string :as str]
            [clojure.xml :as xml :only [emit-element]]))


;; This plugin requires all emission api calls to be
;; forwarded to it.
(config/change-defaults :print-level :print-facts)

(def report-file "report.xml")

(defn log-fn []
  (fn [text] (spit report-file text :append true)))

(defn- log [string]
  (let [log-fn (log-fn)]
    (log-fn string)))

(defn- reset-log []
  (spit report-file ""))

(defn def-fact-cache []
 (defonce last-fact (atom {})))

(defn- fact-name [fact]
  (or (fact/name fact)
      (fact/description fact)
      (str (fact/file fact) ":" (fact/line fact))))

(defn pass []
  (log
    (with-out-str
      (xml/emit-element @last-fact))))

(defn- testcase-with-failure [failure-map]
  (let [testcase @last-fact
        failure-content (str "<![CDATA[" (apply str (lines/summarize failure-map)) "]]>")
        fail-type (:type failure-map)
        fail-element {:tag :failure
                      :content [failure-content]
                      :attrs {:type fail-type}}
        testcase-with-failure (assoc testcase :content [fail-element])]
    testcase-with-failure))

(defn escape [s]
  (if s
    (str/escape s {\" "&quot;"
                   \' "&apos;"
                   \< "&lt;"
                   \> "&gt;"
                   \& "&amp;"})
    ""))

(defn fail [failure-map]
  (let [testcase (testcase-with-failure failure-map)]
    ; FIXME: currently there is a bug in midje that prevents us emitting this map as xml
    ;(xml/emit-element testcase)

    (log (str "<testcase classname='" (-> testcase :attrs :classname) "' name='" (-> testcase :attrs :name)  "'>\n"))
    (log (str "<failure type='" (-> testcase :content first :attrs :type) "'>"))
    (log (-> testcase :content first :content :first))
    (log "</failure>\n")
    (log "</testcase>")))

(defn starting-to-check-fact [fact]
  (let [fact-namespace (str (fact/namespace fact))
        fact-name (fact-name fact)]
    (reset! last-fact {:tag :testcase
                       :attrs {:classname (escape fact-namespace) :name (escape fact-name)}})))

(defn starting-fact-stream []
  (def-fact-cache)
  (reset-log)
  (log "<testsuite>"))

(defn finishing-fact-stream [midje-counters clojure-test-map]
  (log "</testsuite>"))

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))

(def emission-map (merge silence/emission-map
                         (make-map :fail
                                   :pass
                                   :starting-fact-stream
                                   :finishing-fact-stream
                                   :starting-to-check-fact)))

(state/install-emission-map emission-map)
