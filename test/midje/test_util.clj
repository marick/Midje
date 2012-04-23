(ns midje.test-util
  (:use [clojure.test]
        midje.checkers
        [midje.checkers.extended-equality :only [extended-=]]
        [midje.checkers.extended-falsehood :only [extended-false?]]
        midje.error-handling.exceptions
        [clojure.set :only [subset?]]
        [midje.util.form-utils :only [macro-for]]))

(def reported (atom []))

(defn run-without-reporting [f] 
  (binding [report (fn [report-map#] (swap! reported conj report-map#))]
    (reset! reported [])
    (f)))

(defmacro run-silently [run-form]
  `(run-without-reporting (fn [] ~run-form)))

(defmacro after-silently [example-form & check-forms]
   `(do
     (run-without-reporting (fn [] ~example-form))
     ~@check-forms))

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


;; Applied to lists of result maps
(defchecker has-bad-result [reporteds]
  (some (comp not extended-false?) (map bad-result reporteds)))
(defchecker has-wrong-call-count [reporteds]
  (some (comp not extended-false?) wrong-call-count reporteds))
(defn passes [reporteds]
  (every? pass reporteds))


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
  `(after-silently
    ~@body  
    (midje.sweet/fact 
      @reported midje.sweet/=> (one-of (contains {:type :validation-error 
                                                  :notes (contains ~error-msg)})))))

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
