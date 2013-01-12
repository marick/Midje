(ns midje.test-util
  (:use [clojure.test]
        midje.checkers
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.extended-falsehood :only [extended-false?]]
        midje.error-handling.exceptions
        [clojure.set :only [subset?]]
        [midje.util.form-utils :only [macro-for]])
  (:require [midje.clojure-test-facade :as ctf]
            [midje.config :as config]
            [clojure.string :as str]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]))

;;; The "silent" versions of fact and formula, which produce no user-visible results
;;; but do stash failures for later examination.

(def silent-fact:failure-count (atom :unset))
(def silent-fact:raw-failures (atom :unset))
(def ^{:dynamic true} silent-fact:last-raw-failure (atom :unset))

(defn record-failures []
  (reset! silent-fact:failure-count (state/output-counters:midje-failures))
  (reset! silent-fact:raw-failures (state/raw-fact-failures))
  (reset! silent-fact:last-raw-failure (last (state/raw-fact-failures))))


(defn silent-body [macro-symbol args]
  `(emit/producing-only-raw-fact-failures
    (let [result# (~macro-symbol ~@args)]
      (record-failures)
      result#)))

(defmacro silent-fact [& args]
  (silent-body 'midje.sweet/fact args))
(defmacro silent-formula [& args]
  (silent-body 'midje.sweet/formula args))
(defmacro silent-against-background [& args]
  (silent-body 'midje.sweet/against-background args))
(defmacro silent-background [& args]
  (silent-body 'midje.sweet/background args))
(defmacro silent-tabular [& args]
  (silent-body 'midje.sweet/background args))
(defmacro silent-expect [& args]
  (silent-body 'midje.semi-sweet/expect args))
(defmacro silent-fake [& args]
  (silent-body 'midje.semi-sweet/fake args))
(defmacro silent-data-fake [& args]
  (silent-body 'midje.semi-sweet/data-fake args))

;;; Checkers, mainly for failures

(defn fact-fails-because-of-negation [failure-map]
  (some #{(:type failure-map)} [:mock-actual-inappropriately-matches-checker
                                :mock-expected-result-inappropriately-matched]))

(defn fact-failed-with-note [regex]
  (fn [actual-failure]
    (extended-= (str/join (:notes actual-failure)) regex)))

(defn fact-expected [thing]
  (fn [actual-failure]
    (extended-= (:expected actual-failure) thing)))

(defn fact-actual [thing]
  (fn [actual-failure]
    (extended-= (:actual actual-failure) thing)))

(defn fact-captured-throwable-with-message [rhs]
  (fn [actual-failure]
    (extended-= (captured-message (:actual actual-failure)) rhs)))

(defn fact-described-as [& things]
  (fn [actual-failure]
    (= things (:description actual-failure))))

;; Chatty checkers.

(defn intermediate-result-left-match [left failure]
  (first (filter #(= (first %) left) (:intermediate-results failure))))

(defmacro fact-gave-intermediate-result [left _arrow_ value & ignore]
  `(fn [actual-failure#]
     (if-let [match# (intermediate-result-left-match '~left actual-failure#)]
       (= (second match#) ~value))))
      
(defmacro fact-omitted-intermediate-result [left]
  `(fn [actual-failure#]
     (not (intermediate-result-left-match '~left actual-failure#))))
  

;; Prerequisites

(defn ^{:all-failures true} some-prerequisite-was-not-matched [failure-maps]
  (some #{:mock-argument-match-failure} (map :type failure-maps)))

(defn ^{:all-failures true} prerequisite-called [_times_ n]
  (fn [failure-maps]
    (some #(= n (:actual-count %)) (mapcat :failures failure-maps))))

(defn ^{:all-failures true} prerequisite-expected-call [call]
  (fn [failure-maps]
    (some #{call} (map :expected-call  (mapcat :failures failure-maps)))))


;; Misc

(defn parser-exploded [failure-map]
  (= (:type failure-map) :validation-error))



(defmacro note-that [& claims]
  (letfn [(reducer [so-far claim]
            (letfn [(claim-needs-all-failures? [claim]
                      (:all-failures (meta (ns-resolve *ns* claim))))
                    (tack-on [stuff]
                      (concat so-far stuff))
                    (tack-on-claim [all-failures?]
                      (tack-on `(~(if all-failures? '@silent-fact:raw-failures '@silent-fact:last-raw-failure)
                                 midje.sweet/=> ~claim)))]
              (cond (symbol? claim)
                    (cond (or (= claim 'fact-fails)
                              (= claim 'fact-failed))
                          (tack-on '(@silent-fact:failure-count => pos?))
                                      
                          (or (= claim 'fact-passes)
                              (= claim 'fact-passed))
                          (tack-on '(@silent-fact:failure-count => zero?))

                          :else
                          (tack-on-claim (claim-needs-all-failures? claim)))

                    (sequential? claim)
                    (let [claim-symbol (first claim)]
                      (cond (or (= claim-symbol 'fails)
                                (= claim-symbol 'failed))
                            (tack-on `(@silent-fact:failure-count midje.sweet/=> ~(second claim)))

                            :else
                            (tack-on-claim (claim-needs-all-failures? claim-symbol))))

                    :else
                    (throw (Error. (str "What kind of claim is " (pr-str claim)))))))]

    (let [clauses (reduce reducer [] claims)]
      (with-meta `(midje.sweet/fact ~@clauses) (meta &form)))))

(defmacro for-each-failure [note-command]
  `(doseq [failure# @silent-fact:raw-failures]
     (binding [silent-fact:last-raw-failure (atom failure#)]
       ~note-command)))



;;; Capturing output 

(def test-output (atom nil))

(defmacro capturing-output [fact1 fact2]
  `(do
     (reset! test-output
             (with-out-str (state/with-isolated-output-counters ~fact1)))
     ~fact2))
















;;; OLD STUFF. Some worth salvaging.

(defmacro captured-output [& body]
  `(binding [clojure.test/*test-out* (java.io.StringWriter.)]
     (clojure.test/with-test-out ~@body)
     (.toString clojure.test/*test-out*)))

(def reported (atom []))

;; Scheduled for demolition
(defn run-without-reporting [f] 
  (binding [report (fn [report-map#] (swap! reported conj report-map#))
            ctf/report (fn [report-map#] (swap! reported conj report-map#))]
    (reset! reported [])
    (f)))

(defmacro run-silently [& run-forms]
  `(run-without-reporting (fn [] ~@run-forms)))




;; Some sets of tests generate failures. The following code prevents
;; them from being counted as failures when the final summary is
;; printed. The disadvantage is that legitimate failures won't appear
;; in the final summary. They will, however, produce failure output,
;; so that's an acceptable compromise.

(defmacro without-changing-cumulative-totals [& forms]
  `(ctf/ignoring-counter-changes
    (state/with-isolated-output-counters
      ~@forms)))


(defmacro without-externally-visible-changes [& body]
  `(config/with-augmented-config {:print-level :print-nothing}
     (without-changing-cumulative-totals ~@body)))

;; This notes when a test incorrectly stepped on the running
;; count that you see from `lein midje`.
(defmacro confirming-cumulative-totals-not-stepped-on [& body]
  `(let [stashed-counters# (ctf/counters)]
     ~@body
     (midje.sweet/fact "Checking whether cumulative totals were stepped on"
       (>= (:pass (ctf/counters)) (:pass stashed-counters#)) midje.sweet/=> true
       (>= (:fail (ctf/counters)) (:fail stashed-counters#)) midje.sweet/=> true)))

















;; (defmacro after-silently [example-form & check-forms]
;;    `(do
;;      (run-without-reporting (fn [] ~example-form))
;;      ~@check-forms))

(defn only-passes? [expected-count]
  (cond (not (= (count @reported) expected-count))
        (do 
          (println "Count" (count @reported) "when" expected-count "expected")
          (println @reported)
          false)

        (not (= (count (filter #(= (:type %) :pass) @reported)) expected-count))
        (do
          (println "Not everything passed.")
          (println "Actual" @reported)
          false)

        :else
        true))

(defn raw-report [] (println @reported) true)

(defmacro in-separate-namespace [& forms]
  `(let [old-ns# *ns*]
    (try (in-ns (gensym)) ~@forms
    (finally (in-ns (ns-name old-ns#))))))

;; Kinds of result maps
(def bad-result (contains {:type :mock-expected-result-failure}))
(def inappropriate-equality (contains {:type :mock-expected-result-inappropriately-matched}))
(def inappropriate-checker (contains {:type :mock-actual-inappropriately-matches-checker}))
(def pass (contains {:type :pass}))
(def checker-fails (contains {:type :mock-expected-result-functional-failure}))
(def wrong-call-count (contains {:type :mock-incorrect-call-count}))
(def a-validation-error (contains {:type :validation-error}))
(def no-matching-prerequisite (contains {:type :mock-argument-match-failure}))
(def future-fact-note (contains {:type :future-fact}))


;; Applied to lists of result maps
(letfn [(make-collection-checker [unit-checker]
          (checker [reporteds]
            (some (comp not extended-false?) (map unit-checker reporteds))))]
  (defchecker has-bad-result [reporteds]
    (make-collection-checker bad-result))
  (defchecker has-wrong-call-count [reporteds]
    (make-collection-checker wrong-call-count))

  (defn passes [reporteds]
    (every? pass reporteds))
)


(defchecker has-thrown-message [expected]
  (checker [reporteds]
    (some (fn [one-report]
            (and (:actual one-report)
                 (captured-throwable? (:actual one-report))
                 (extended-= (.getMessage (throwable (:actual one-report)))
                             expected)))
          reporteds)))


(defn at-line [line-no form] 
   (with-meta form {:line line-no}))

(defmacro validation-error-with-notes [& notes]
  `(just (contains {:notes (just ~@notes)
                    :type :validation-error})))

(defmacro causes-validation-error 
  "check if the body, when executed, creates a syntax validation error"
  [error-msg & body]
  (let [metadata (meta &form)]
    `(do (with-meta (silent-fact ~@body) '~metadata)
         (note-that parser-exploded (fact-failed-with-note ~error-msg)))))

(defmacro each-causes-validation-error 
  "check if each row of the body, when executed, creates a syntax validation error"
  [error-msg & body]
  (macro-for [row body]
    `(causes-validation-error ~error-msg ~row)))

(defmacro with-identity-renderer [& forms]
  `(binding [midje.ideas.reporting.report/*renderer* identity] ~@forms))

(defmacro defn-call-countable
  "Note: For testing Midje code that couldn't use provided.
  
  Creates a function that records how many times it is called, and records 
  that count in an atom with the same name as the function with \"-count\" appended"
  [name args & body]
  (let [atom-name (symbol (str name "-count"))]
    `(do
       (def ~atom-name (atom 0))
       (defn ~name ~args
         (swap! ~atom-name inc)
         ~@body))))




