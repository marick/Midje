
(ns midje.semi-sweet
  (:use clojure.test)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.error-kit)
)


  (defn- position-string [position-pair]
    (format "(%s:%d)" (first position-pair) (second position-pair)))


  (defmethod clojure.test/report :mock-argument-match-failure [m]
    (with-test-out
     (inc-report-counter :fail)
     (println "\nFAIL near" (midje.semi-sweet/position-string (:position m)))
     (when (seq *testing-contexts*) (println (testing-contexts-str)))
     (println "You never said" (:name (meta (:function m))) "would be called with these arguments:")
     (println (pr-str (:actual m)))))

  (defmethod clojure.test/report :mock-incorrect-call-count [m]
    (with-test-out
     (inc-report-counter :fail)
     (println "\nFAIL for" (midje.semi-sweet/position-string (:position m)))
     (when (seq *testing-contexts*) (println (testing-contexts-str)))
     (println "This expectation was never satisfied:")
     (println (:expected m) "should be called at least once.")))


  (defmethod clojure.test/report :mock-expected-result-failure [m]
    (with-test-out
     (inc-report-counter :fail)
     (println "\nFAIL near" (midje.semi-sweet/position-string (:position m)))
     (when (seq *testing-contexts*) (println (testing-contexts-str)))
     (println "expected:" (pr-str (:expected m)))
     (println "  actual:" (pr-str (:actual m)))))

(def *file-position-of-call-under-test* nil)

  "Raising a failure-during-computation means 'don't check expectations'."
(deferror *failure-during-computation* [] [])


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

(defn- contains-element?
  "Is the element in the sequence? Works for vectors."
  [seq elt]
  (some #{elt} seq))

(defn- midje-file? [position]
  (contains-element? ["semi_sweet.clj"] (first position))
)

(defn- without-java [positions]
  (remove (fn [position] (re-find #"\.java" (first position)))
	  positions))

(defn- strip-leading-infrastructure [positions]
  (let [ [clojure-core midje-files & more-partitions] (partition-by midje-file? positions)]
    (first more-partitions))
)

(defn user-file-position []
  (try
   (let [integers (iterate inc 1)
	 positions (without-java (map file-position integers))
	 above-midje-files (strip-leading-infrastructure positions)
	 ]
     (first above-midje-files))
   (catch Exception e ["unknown file" 0]))
)

(defn- unique-function-vars [expectations]
  (distinct (map #(:function %) expectations))
)

(defn- find-matching-call [faked-function args expectations]
  (find-first (fn [expectation] 
		  (and (= faked-function (expectation :function))
		       (= (count args) (count (expectation :arg-matchers)))
		       (matching-args? args (expectation :arg-matchers))))
	      expectations)
)



(defn call-faker [faked-function args expectations]
  (let [found (find-matching-call faked-function args expectations)]
    (if-not found 
	    (do 
	      (report {:type :mock-argument-match-failure
		      :function faked-function
		      :actual args
		      :position (:file-position (first expectations))})
	      (raise *failure-during-computation*))
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

(defn- check-call-counts [expectations]
  (doseq [expectation expectations]
      (if (zero? @(expectation :count-atom))
	(do
	  (report {:type :mock-incorrect-call-count
		   :expected-call (expectation :call-text-for-failures)
		   :position (:file-position expectation)
		   :expected (expectation :call-text-for-failures)})
	  (raise *failure-during-computation*))))
	  
)

; TODO: (expect calls need to record their file position)
(defn- check-result [actual expected expectations]
  (if-not (= actual expected)
     (report {:type :mock-expected-result-failure
	      :position (:file-position (first expectations))
	      :actual actual
	      :expected expected }))
)


(defmacro #^{:private true} short-circuiting-failure [tested-fn expected-result expectations]
  `(with-handler (let [code-under-test-result# (eagerly (~tested-fn))]
		   (check-call-counts ~expectations)
		   (check-result code-under-test-result# ~expected-result ~expectations))
		 (handle *failure-during-computation* []))
)

(defn arg-matcher-maker [expected]
  (fn [actual] (= actual expected)))

(defmacro fake 
  "Creates an expectation that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  [call-form ignored result]
  `{:function (def ~(first call-form))
    :arg-matchers (map arg-matcher-maker [~@(rest call-form)])
    :call-text-for-failures (str '~call-form)
    :result-supplier (fn [] ~result)
    :count-atom (atom 0)
    :file-position (user-file-position)}
)


(defn expect* [call-fn expected-result expectations]
  (with-bindings (binding-map expectations)
      (short-circuiting-failure call-fn expected-result expectations))
  )

(defmacro expect 
  "doc string here"
  ([call-form ignored expected-result]
   `(expect ~call-form ~ignored ~expected-result []))
  ([call-form ignored expected-result expectations]
   `(binding [*file-position-of-call-under-test* (user-file-position)]
      (expect* (fn [] ~call-form) ~expected-result ~expectations)))
)


