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


(defn finishing-fact-stream [midje-counters clojure-test-map]
  (letfn [(midje-summary-lines [passes fails]
            (letfn [(midje-failure-summary []
                      (if (= 1 fails)
                        (str (color/fail "FAILURE:") (format " %d check failed." fails))
                        (str (color/fail "FAILURE:") (format " %d checks failed." fails))))
          
                    (midje-consolation []
                      (condp = passes
                        0 ""
                        (format " (But %d succeeded.)" passes)))]
              
              (vector
               (cond (zero? (+ passes fails))
                     (color/note "No facts were checked. Is that what you wanted?")
                     
                     (zero? fails)
                     (color/pass (format "All checks (%d) succeeded." passes))
                     
                     :else
                     (str (midje-failure-summary) " " (midje-consolation))))))

          (clojure-test-prompted-lines []
            (when (pos? (:test clojure-test-map))
              (let [lines (:lines clojure-test-map)
                    result-lines (drop-last 2 lines)
                    summary-line (last lines)
                    grievousness? (pos? (+ (:fail clojure-test-map) (:error clojure-test-map)))]
                (concat [(color/note ">>> Output from clojure.test tests:")]
                        (map #(-> % (str/replace #"^FAIL" (color/fail "FAIL"))
                                  (str/replace #"^ERROR" (color/fail "ERROR")))
                             result-lines)
                        [((if grievousness? color/fail color/pass) summary-line)]
                        [(color/note ">>> Midje summary:")]))))]

    (apply util/emit-one-line (concat (clojure-test-prompted-lines)
                                      (midje-summary-lines (:midje-passes midje-counters) (:midje-failures midje-counters))))))

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
                                   :future-fact
                                   :starting-to-check-fact
                                   :possible-new-namespace
                                   :finishing-fact-stream
                                   :starting-fact-stream)))
  
(state/install-emission-map emission-map)
