(ns ^{:doc "JUnit formatter for Midje output"}
  midje.emission.plugins.junit
  (:import java.io.File)
  (:require [clj-time.core :as time]
            [clojure.string :as string]
            [clojure.xml :as xml :only [emit-element]]
            [midje.config :as config]
            [midje.data.fact :as fact]
            [midje.emission.util :refer :all]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as silence]
            [midje.emission.plugins.default-failure-lines :as lines]))


;; This plugin requires all emission api calls to be
;; forwarded to it.
(config/change-defaults :print-level :print-facts
                        :colorize false)

(defonce report-file (atom nil))
(defonce last-fact (atom {}))
(defonce last-ns (atom nil))

;; this is purely for use in test cases - need to provide a sideeffect
(defn- log-fn []
  (fn [text] (spit @report-file (str text "\n") :append true)))

(defn- log [string]
  (let [log-fn (log-fn)]
    (log-fn string)))

(defn escape [s]
  (if s
    (string/escape s {\" "&quot;"
                      \' "&apos;"
                      \< "&lt;"
                      \> "&gt;"
                      \& "&amp;"})
    ""))

(defn- make-report-filename [ns]
  (.mkdir (File. "target/surefire-reports"))
  (str "target/surefire-reports/TEST-" (escape (str ns)) ".xml"))

(defn- close-report []
  (when @report-file
    (log "</testsuite>"))
  (reset! report-file nil))

;; separate from open-report for easier testing
(defn- clear-file [filename]
  (spit filename ""))

(defn- open-report [ns]
  (when @report-file
    (close-report))
  (reset! report-file (make-report-filename ns))
  (clear-file @report-file)
  (log (str "<testsuite name='" (escape (str ns)) "'>")))

(defn- fact-name [fact]
  (or (fact/name fact)
      (fact/description fact)
      (str (fact/file fact) ":" (fact/line fact))))

(defn process-fact [fact]
  (let [elapsed (/ (time/in-millis (time/interval (-> fact :attrs :start-time)
                                                  (-> fact :attrs :stop-time)))
                   1000.0)
        dissoc-times (fn [attrs] (-> attrs
                                    (dissoc :start-time)
                                    (dissoc :stop-time)))]
    (-> fact
        (update-in [:attrs] dissoc-times)
        (assoc-in [:attrs :time] elapsed))))

(defn pass []
  (log
    (with-out-str
      (xml/emit-element (process-fact @last-fact)))))

(defn- testcase-with-failure [failure-map]
  (let [testcase (process-fact @last-fact)
        failure-content (str "<![CDATA[" (apply str (lines/summarize failure-map)) "]]>")
        fail-type (:type failure-map)
        fail-element {:tag :failure
                      :content [failure-content]
                      :attrs {:type fail-type}}
        testcase-with-failure (assoc testcase :content [fail-element])]
    testcase-with-failure))

(defn fail [failure-map]
  (let [testcase (testcase-with-failure failure-map)]
    ;; FIXME: currently there is a bug in midje that prevents us emitting this map as xml
    ;; (xml/emit-element testcase) => StackOverflowError, when called in a
    ;; midje namespace
    (log (str "<testcase classname='" (-> testcase :attrs :classname) "' name='" (-> testcase :attrs :name)  "'>\n"))
    (log (str "<failure type='" (-> testcase :content first :attrs :type) "'>"))
    (log (-> testcase :content first :content first))
    (log "</failure>\n")
    (log "</testcase>")))

(defn starting-to-check-fact [fact]
  (let [fact-namespace (str (fact/namespace fact))
        fact-name (fact-name fact)]
    (reset! last-fact {:tag :testcase
                       :attrs {:classname (escape fact-namespace)
                               :name (escape fact-name)
                               :start-time (time/now)}})))

(defn finishing-fact [fact]
  (swap! last-fact assoc-in [:attrs :stop-time] (time/now)))

(defn possible-new-namespace [ns]
  (when-not (= @last-ns ns)
    (open-report ns))
  (reset! last-ns ns))

(defn finishing-fact-stream [midje-counters clojure-test-map]
  (close-report))

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))

(def emission-map (merge silence/emission-map
                         (make-map :fail
                                   :pass
                                   :possible-new-namespace
                                   :finishing-fact-stream
                                   :starting-to-check-fact
                                   :finishing-fact)))

(state/install-emission-map emission-map)
