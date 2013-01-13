(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:use [midje.util.form-utils :only [midje-position-string]])
  (:require [midje.util.colorize :as color]
            [midje.emission.state :as state]
            [midje.ideas.metadata :as metadata]
            [midje.emission.plugins.util :as util]
            [midje.emission.plugins.silence :as silence]
            [midje.clojure-test-facade :as ctf]
            [clojure.string :as str]))

(defn fail [report-map]
  (ctf/ignoring-counter-changes
    (clojure.test/report report-map)))


(def last-namespace-shown (atom nil))

(defn set-last-namespace-shown! [string]
  (reset! last-namespace-shown string))

(defn possible-new-namespace [namespace-symbol]
  (when (not= namespace-symbol @last-namespace-shown)
    (util/output (color/note (str "= Namespace " namespace-symbol)))
    (set-last-namespace-shown! namespace-symbol)))

(defn forget-everything []
  (set-last-namespace-shown! nil))

(defn starting-to-check-fact [fact-function]
  (util/output (color/note
               (str "Checking "
                    (or (metadata/fact-name fact-function)
                        (metadata/fact-description fact-function)
                        (str "fact at " (midje-position-string
                                         [(metadata/fact-file fact-function)
                                          (metadata/fact-line fact-function)])))))))


(defn fact-stream-summary [midje-counters clojure-test-map]
  (letfn [(midje-summary-lines [passes fails]
            (letfn [(midje-failure-summary []
                      (if (= 1 fails)
                        (str (color/fail "FAILURE:") (format " %d claim was not confirmed." fails))
                        (str (color/fail "FAILURE:") (format " %d claims were not confirmed." fails))))
          
                    (midje-consolation []
                      (condp = passes
                        0 ""
                        1 "(But 1 was.)"
                        (format " (But %d were.)" passes)))]
              
              (vector
               (cond (zero? (+ passes fails))
                     (color/note "No facts were checked. Is that what you wanted?")
                     
                     (zero? fails)
                     (color/pass (format "All claims (%d) have been confirmed." passes))
                     
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
                        [""]
                        [((if grievousness? color/fail color/pass) summary-line)]
                        [(color/note ">>> Midje summary:")]))))]

    (apply util/output (concat (clojure-test-prompted-lines)
                               (midje-summary-lines (:midje-passes midje-counters) (:midje-failures midje-counters))))))

(defn future-fact [report-map]
  (clojure.test/report report-map))
  

(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))


(def emission-map (merge silence/emission-map
                         (make-map ;; `:pass` takes no arguments. In this case,
                                   ;; it's defined in the silence plugin.
                                   ;; Note that the count of passes and fails
                                   ;; is maintained in `state`, so you needn't
                                   ;; do it yourself.
                                   ;; :pass

                                   ;; `:fail` takes a map of information. See
                                   ;; `default-fails` for the different sorts
                                   ;; of maps and how they're handled.
                                   ;; Note that the count of passes and fails
                                   ;; is maintained in `state`, so you needn't
                                   ;; do it yourself.
                                   :fail

                                   ;; `:future-fact` takes a map similar to
                                   ;; `fail`. The key fields are:
                                   ;;    :position     (file/line)
                                   ;;    :description  (text describing the future)
                                   ;; Note that the discription may be nil.
                                   :future-fact
                                   
                                   ;; `:starting-to-check-fact` takes a fact-function
                                   ;; as an argument. Use its metadata to construct
                                   ;; output. See the function above for an example.
                                   :starting-to-check-fact

                                   ;; `:possible-new-namespace` is called whenever a
                                   ;; namespace might change, most notably whenever
                                   ;; a new fact is being checked.
                                   :possible-new-namespace

                                   ;; `:fact-stream-summary is called whenever a
                                   ;; series (or "stream") of facts has been checked.
                                   ;; The first argument is a counter with these fields:
                                   ;;   :midje-passes
                                   ;;   :midje-failures
                                   ;; Note: Midje doesn't have a notion of a count of tests
                                   ;; that were run. You can simulate it via
                                   ;; `:starting-to-check-fact` or `:starting-to-check-top-level-fact`.
                                   ;;
                                   ;; The second argument is a map containing the normal
                                   ;; clojure.test keys, which are:
                                   ;;       :test           (count of tests run)
                                   ;;       :pass
                                   ;;       :fail           (comparison failure)
                                   ;;       :error          (exception in test)
                                   ;; Because Midje buffers up clojure.test output, the second
                                   ;; argument contains an additional key:
                                   ;;       :lines          (output lines from test run)
                                   ;; Note: Midje doesn't always run clojure.test tests. If it 
                                   ;; didn't, the second argument is a map with a single key, `:test`,
                                   ;; whose value is 0.
                                   :fact-stream-summary

                                   ;; `:forget-everything` is called when a new
                                   ;; "stream" of facts is to be evaluated. The most
                                   ;; typical case is when `load-facts` is called,
                                   ;; either directly or via `lein midje`. Note that
                                   ;; the running count of passes and fails is zeroed
                                   ;; before this call.
                                   :forget-everything)))
  
(state/install-emission-map emission-map)
