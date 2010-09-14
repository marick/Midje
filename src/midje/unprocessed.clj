(ns midje.unprocessed
  (:use clojure.test
        midje.report
        clojure.contrib.error-kit
        [clojure.contrib.ns-utils :only [immigrate]]
        [clojure.contrib.seq-utils :only [find-first]]))
(immigrate 'midje.checkers)



(deferror one-failure-per-test [] [])  ; Check nothing further


(defn- pairs [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn- eagerly [value]
  (if (seq? value)
    (doall value)
    value))

(defn- matching-args? [actual-args matchers]
  (every? (fn [ [actual matcher] ] (matcher actual))
   	  (pairs actual-args matchers))
)

(defn user-file-position []
  "Guesses the file position (basename and line number) that the user is
   most likely to be interested in if a test fails."
  (second (map #(list (.getFileName %) (.getLineNumber %))
               (.getStackTrace (Throwable.))))
)

(defmacro line-number-known [number]
  `[(first (user-file-position)) ~number])

(defn- unique-function-vars [expectations]
  (distinct (map #(:function %) expectations))
)

(defmulti matches-call? (fn [expectation faked-function args]
                          (:type expectation)))

(defmethod matches-call? :not-called
  [expectation faked-function args]
  (= faked-function (expectation :function)))

(defmethod matches-call? :default
  [expectation faked-function args]
  (and (= faked-function (expectation :function))
       (= (count args) (count (expectation :arg-matchers)))
       (matching-args? args (expectation :arg-matchers))))


(defn- find-matching-call [faked-function args expectations]
  (find-first #(matches-call? % faked-function args) expectations)
)

(defn call-faker [faked-function args expectations]
  "This is the function that handles all mocked calls. Don't use it."
  (let [found (find-matching-call faked-function args expectations)]
    (if-not found 
      (do 
        (report {:type :mock-argument-match-failure
                 :function faked-function
                 :actual args
                 :position (:file-position (first expectations))})
        (raise one-failure-per-test))
      (do 
        (swap! (found :count-atom) inc)
        ((found :result-supplier)))))
)

(defn- binding-map [expectations]
  (reduce (fn [accumulator function-var] 
	      (let [faker (fn [& actual-args] (call-faker function-var actual-args expectations))]
		(assoc accumulator function-var faker)))
	  {}
	  (unique-function-vars expectations))
)

(defn- function-aware-= [actual expected]
  (if (fn? expected) 
    (expected actual)
    (= actual expected))
)

(defmulti call-count-incorrect? :type)

(defmethod call-count-incorrect? :fake
  [expectation]
  (zero? @(expectation :count-atom)))

(defmethod call-count-incorrect? :not-called
  [expectation]
  (not (zero? @(expectation :count-atom))))

(defmethod call-count-incorrect? :default
  [expectation]
  false)

(defn- check-call-counts [expectations]
  (doseq [expectation expectations]
    (if (call-count-incorrect? expectation)
      (do
        (report {:type :mock-incorrect-call-count
                 :expected-call (expectation :call-text-for-failures)
                 :position (:file-position expectation)
                 :expected (expectation :call-text-for-failures)})
        (raise one-failure-per-test))))
)

;; TODO: I'm not wild about signalling failure in two ways: by report() and by
;; return value. Fix this when (a) we move away from clojure.test.report and
;; (b) we figure out how to make fact() some meaningful unit of reporting.
(defn- check-result [actual call expectations]
  (cond (function-aware-= actual (call :expected-result))
   	  (do (report {:type :pass})
	      true)
	(fn? (call :expected-result))
	  (do (report {:type :mock-expected-result-functional-failure
		       :position (call :file-position)
		       :actual actual
		       :expected (call :expected-result-text-for-failures) })
	      false)
	:else
	  (do 
	    (report {:type :mock-expected-result-failure
		     :position (call :file-position)
		     :actual actual
		     :expected (call :expected-result) })
	    false))
)


(defn arg-matcher-maker [expected]
  "Based on an expected value, generates a function that returns true if the 
   actual value matches it. Don't use this."
  (fn [actual] (function-aware-= actual expected)))

(defmacro #^{:private true} stopping-upon-mock-failures [form]
  `(with-handler ~form
     (handle one-failure-per-test [] false)))

(defn- is-error-kit-throwable? [e]   
  (re-find #"^Error Kit Control Exception" (.toString e))) ; Ick.

(defmacro capturing-exception [form]
  `(try ~form
	(catch Throwable e#
	  (if (is-error-kit-throwable? e#)
	    (throw e#)
	    (midje.checkers/captured-exception e#)))))

(defn expect* [call-map expectations]
  "The core function in unprocessed Midje. Takes a map describing a call and a 
   list of maps, each of which describes a secondary call the first call is supposed to 
   make. See the documentation at http://github.com/marick/Midje."
  (with-bindings (binding-map expectations)
     (stopping-upon-mock-failures
      (let [code-under-test-result (capturing-exception (eagerly ((call-map :function-under-test))))]
	(check-call-counts expectations)
	(check-result code-under-test-result call-map expectations)))))
      

