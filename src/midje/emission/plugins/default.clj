(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:require [midje.data.fact :as fact]
            [midje.emission.colorize :as color]
            [midje.emission.plugins.util :as util]
            [midje.emission.plugins.silence :as silence]
            [midje.emission.plugins.default-failure-lines :as lines]
            [midje.emission.state :as state]
            [midje.emission.util :refer :all]
            [clojure.string :as str]))

(defn fail [failure-map]
  (util/emit-lines (lines/summarize failure-map)))

(defn info [lines]
  (util/emit-lines lines))

(def last-namespace-shown (atom nil))

(defn set-last-namespace-shown! [string]
  (reset! last-namespace-shown string))

(defn possible-new-namespace [namespace-symbol]
  (when (not= namespace-symbol @last-namespace-shown)
    (util/emit-one-line (color/note (str "= Namespace " namespace-symbol)))
    (set-last-namespace-shown! namespace-symbol)))

(defn starting-fact-stream []
  (set-last-namespace-shown! nil))

(defn starting-to-check-fact [fact]
  (util/emit-one-line (color/note
                       (str "Checking "
                            (or (fact/name fact)
                                (fact/description fact)
                                (str "fact at " (midje-position-string
                                                 [(fact/file fact)
                                                  (fact/line fact)])))))))


(defn- midje-summary-line [passes fails]
  (cond (zero? (+ passes fails))
        (color/note "No facts were checked. Is that what you wanted?")

        (zero? fails)
        (color/pass (format "All checks (%d) succeeded." passes))

        :else
        (let [fail-summary (if (= 1 fails)
                             (str (color/fail "FAILURE:") (format " %d check failed." fails))
                             (str (color/fail "FAILURE:") (format " %d checks failed." fails)))
              consolation (condp = passes
                            0 ""
                            (format " (But %d succeeded.)" passes))]
          (str fail-summary " " consolation))))

(defn- clojure-test-prompted-lines [clojure-test-map]
  (when (pos? (:test clojure-test-map))
    (let [lines                       (:lines clojure-test-map)
          result-lines                (drop-last 2 lines)
          [success-line failure-line] (take-last 2 lines)
          grievousness?               (pos? (+ (:fail clojure-test-map)
                                               (:error clojure-test-map)))
          emit-result                 (if grievousness? color/fail color/pass)]
      (concat [(color/note "\n>>> Output from clojure.test tests:")]
              (map #(-> %
                        (str/replace #"^FAIL" (color/fail "FAIL"))
                        (str/replace #"^ERROR" (color/fail "ERROR")))
                   result-lines)
              [(emit-result success-line)
               (emit-result failure-line)]))))

(defn finishing-fact-stream [midje-counters clojure-test-map]
  (let [clojure-test-lines (clojure-test-prompted-lines clojure-test-map)]
    (apply util/emit-one-line
           (concat [(when clojure-test-lines
                      (color/note ">>> Midje summary:"))
                    (midje-summary-line (:midje-passes midje-counters)
                                        (:midje-failures midje-counters))]
                   clojure-test-lines))))

(defn future-fact [description-list position]
  (util/emit-one-line "")
  (util/emit-one-line (str (color/note "WORK TO DO") " "
                           (when-let [doc (util/format-nested-descriptions description-list)]
                             (str (pr-str doc) " "))
                           "at " (util/filename-lineno position))))

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))


(def emission-map (merge silence/emission-map
                         (make-map :fail
                                   :info
                                   :future-fact
                                   :starting-to-check-fact
                                   :possible-new-namespace
                                   :finishing-fact-stream
                                   :starting-fact-stream)))

(state/install-emission-map emission-map)
