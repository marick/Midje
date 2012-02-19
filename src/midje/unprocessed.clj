;; -*- indent-tabs-mode: nil -*-

(ns ^{:doc "Core Midje functions that process expects and report on their results."} 
  midje.unprocessed 
  (:use clojure.test
        [midje.ideas.background :only [background-fakes]]
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.chatty :only [chatty-checker?]]
        [midje.error-handling.exceptions :only [captured-throwable]]
        midje.internal-ideas.fakes
        [midje.internal-ideas.fact-context :only [nested-fact-description]]
         midje.internal-ideas.report
         midje.util.laziness
        [midje.util.namespace :only [immigrate]]
        [utilize.seq :only [find-first]]))
(immigrate 'midje.checkers)

(def ^{:private true} formula-reports (atom []))

(defn ^{:private true} report-formula [report-map]
  (swap! formula-reports conj report-map))

(defn ^{:private true} report-formula-conclusion [report-map]
  (let [all-report-maps (conj @formula-reports report-map)]
    (if-let [failure (find-first #(not= :pass (:type %)) all-report-maps)]
      (report {:type :formula-fail :first-failure failure})
      (report {:type :pass}) )
    (reset! formula-reports [])))

(letfn [(fail [type actual call]
          {:type type
           :description (nested-fact-description)
           :binding-note (:binding-note call)
           :position (:position call)
           :actual actual
           :expected (:expected-result-text-for-failures call)})

  (check-result-positive [report-fn actual call]
    (cond (extended-= actual (:expected-result call))
      (report-fn {:type :pass})

      (chatty-checker? (:expected-result call))
      (report-fn (merge (fail :mock-expected-result-functional-failure actual call)
                   (let [chatty-result ((:expected-result call) actual)]
                     (if (map? chatty-result)
                       chatty-result
                       {:notes ["Midje program error. Please report."
                                (str "A chatty checker returned "
                                  (pr-str chatty-result)
                                  " instead of a map.")]}))))

      (fn? (:expected-result call))
      (report-fn (fail :mock-expected-result-functional-failure actual call))

      :else
      (report-fn (assoc (fail :mock-expected-result-failure actual call)
                       :expected (:expected-result call)))))  

  (check-result-negated [report-fn actual call]
    (cond (not (extended-= actual (:expected-result call)))
      (report-fn {:type :pass})

      (fn? (:expected-result call))
      (report-fn (fail :mock-actual-inappropriately-matches-checker actual call))

      :else
      (report-fn (fail :mock-expected-result-inappropriately-matched actual call))))]


  (defmulti ^{:private true} check-result (fn [actual call]
                                            [(:desired-check call) (:formula call)] ))

  (defmethod check-result [:check-match nil] [actual call]
    (check-result-positive report actual call))

  (defmethod check-result [:check-negated-match nil] [actual call]
    (check-result-negated report actual call))

  (defmethod check-result [:check-match :formula-in-progress] [actual call]
    (check-result-positive report-formula actual call))

  (defmethod check-result [:check-negated-match :formula-in-progress] [actual call]
    (check-result-negated report-formula actual call))

  (defmethod check-result [:check-match :formula-conclude] [actual call]
    (check-result-positive report-formula-conclusion actual call))

  (defmethod check-result [:check-negated-match :formula-conclude] [actual call]
    (check-result-negated report-formula-conclusion actual call)))

(defn expect*
  "The core function in unprocessed Midje. Takes a map describing a
  call and a list of maps, each of which describes a secondary call
  the first call is supposed to make. See the documentation at
  http://github.com/marick/Midje."
  [unprocessed-check local-fakes]
  (with-installed-fakes (concat (reverse (filter :data-fake (background-fakes))) local-fakes)
    (let [code-under-test-result (try
                                   (eagerly
                                     ((:function-under-test unprocessed-check)))
                                  (catch Throwable ex
                                    (captured-throwable ex)))]
      (check-call-counts local-fakes)
      (check-result code-under-test-result unprocessed-check)
      :irrelevant-return-value)))