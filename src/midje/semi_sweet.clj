(ns midje.semi-sweet
  (:use clojure.test)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.error-kit)
)


(def *file-position-of-call-under-test* nil)

  "Raising a failure-during-computation means 'don't check expectations'."
(deferror *failure-during-computation* [] [])


(defn- pairs [first-seq second-seq]
  (partition 2 (interleave first-seq second-seq)))

(defn- position-string [position-pair]
  (format "(%s:%d)" (first position-pair) (second position-pair))
)

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
	      (report {:type :fail :subtype :unexpected-call
		      :message (str "The form being tested near "
				    (position-string *file-position-of-call-under-test*)
				    " made an unexpected call.")
		      :expected "Call matching a predefined expectation"
		      :actual (cons faked-function args)})
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
	  (report {:type :fail :subtype :incorrect-call-count
		   :message (str "The form being tested near "
				 (position-string *file-position-of-call-under-test*)
				 " didn't make a call that was expected.")
		   :expected (str (expectation :call-text-for-failures) " should be called once.")
		   :actual "Never called"})))
)


(defmacro #^{:private true} short-circuiting-failure 
  ([form expected]
   `(short-circuiting-failure ~form ~expected nil))

  ([form expected failure-message]
  `(with-handler (let [code-under-test-result# (eagerly ~form)]
		   (is (= code-under-test-result# ~expected) ~failure-message))
		 (handle *failure-during-computation* [])))
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


(defn during* [call-fn expected-result expectations]
  (with-bindings (binding-map expectations)
      (short-circuiting-failure (call-fn) expected-result))
  (check-call-counts expectations))

(defmacro during 
  "doc string here"
  ([call-form ignored expected-result expectations]
   `(binding [*file-position-of-call-under-test* (user-file-position)]
      (during* (fn [] ~call-form) ~expected-result ~expectations)))
)


