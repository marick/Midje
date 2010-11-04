(ns midje.unprocessed.unprocessed-internals
  (:use [clojure.contrib.seq-utils :only [find-first]]
	clojure.test
        clojure.contrib.error-kit
        midje.util.report
	[midje.util.checkers :only [chatty-checker-falsehood? chatty-checker?]]
	)
  (:require [clojure.zip :as zip])
)

(defn pairs [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn eagerly [value]
  (if (seq? value)
    (loop [loc (zip/seq-zip value)]  ;; touch every node
      (if (zip/end? loc)
	(zip/root loc)
	(recur (zip/next loc))))
    value))

(defn matching-args? [actual-args matchers]
  (every? (fn [ [actual matcher] ] (matcher actual))
   	  (pairs actual-args matchers))
)

(defn unique-function-vars [expectations]
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


(defn find-matching-call [faked-function args expectations]
  (find-first #(matches-call? % faked-function args) expectations)
)

(defn call-faker [faked-function args expectations]
  "This is the function that handles all mocked calls. Don't use it."
  (let [found (find-matching-call faked-function args expectations)]
    (if-not found 
      (do 
        (clojure.test/report {:type :mock-argument-match-failure
                 :function faked-function
                 :actual args
                 :position (:file-position (first expectations))}))
      (do 
        (swap! (found :count-atom) inc)
        ((found :result-supplier)))))
  )

(def unbound-marker :midje/special-midje-unbound-marker)

(defn restore-one-root [[variable new-value]]
  (if (= new-value unbound-marker)
    (.unbindRoot variable)
    (alter-var-root variable (fn [current-value] new-value))))

(defn alter-one-root [[variable new-value]]
   (if (bound? variable) 
     (let [old-value (deref variable)]
       (alter-var-root variable (fn [current-value] new-value))
       [variable old-value])
     (do
       (.bindRoot variable new-value)
       [variable unbound-marker])))

(defn with-altered-roots* [binding-map function]
  (let [old-bindings (into {} (for [pair binding-map] (alter-one-root pair)))]
    (try (function)
	 ;; Can't use doseq inside a finally clause.
	 (finally (dorun (map restore-one-root old-bindings))))))

(defmacro with-altered-roots
  "Used instead of with-bindings because bindings are thread-local
   and will require specially declared vars in Clojure 1.3"
  [binding-map & rest]
  `(with-altered-roots* ~binding-map (fn [] ~@rest)))

(defn binding-map [expectations]
  (reduce (fn [accumulator function-var] 
	      (let [faker (fn [& actual-args] (call-faker function-var actual-args expectations))]
		(assoc accumulator function-var faker)))
	  {}
	  (unique-function-vars expectations))
)

(defn function-aware-= [actual expected]
  (if (fn? expected) 
    (let [function-result (expected actual)]
      (if (chatty-checker-falsehood? function-result) false function-result))
    (= actual expected))
)

(defmulti call-count-incorrect? :type)

(defmethod call-count-incorrect? :fake
  [expectation]
  (zero? @(expectation :count-atom)))

(defmethod call-count-incorrect? :not-called
  [expectation]
  (not (zero? @(expectation :count-atom))))

(defmethod call-count-incorrect? :background
  [expectation]
  false)

(defn check-call-counts [expectations]
  (doseq [expectation expectations]
    (if (call-count-incorrect? expectation)
      (do
        (report {:type :mock-incorrect-call-count
                 :expected-call (expectation :call-text-for-failures)
                 :position (:file-position expectation)
                 :expected (expectation :call-text-for-failures)}))))
)

;; TODO: I'm not wild about signalling failure in two ways: by report() and by
;; return value. Fix this when (a) we move away from clojure.test.report and
;; (b) we figure out how to make fact() some meaningful unit of reporting.
(defn check-result [actual call]
  (cond (function-aware-= actual (call :expected-result))
	(do (report {:type :pass})
	    true)

	(fn? (call :expected-result))
	(do (report (merge {:type :mock-expected-result-functional-failure
			    :position (call :file-position)
			    :expected (call :expected-result-text-for-failures) }
			   (if (chatty-checker? (call :expected-result))
			     ((call :expected-result) actual)
			     {:actual actual})))
	    false)
	
	:else
	(do 
	  (report {:type :mock-expected-result-failure
		   :position (call :file-position)
		   :actual actual
		   :expected (call :expected-result) })
	  false))
)

(defmacro capturing-exception [form]
  `(try ~form
	(catch Throwable e#
	  (midje.util.checkers/captured-exception e#))))

(defn arg-matcher-maker [expected]
  "Based on an expected value, generates a function that returns true if the 
   actual value matches it."
  (fn [actual] (function-aware-= actual expected)))

