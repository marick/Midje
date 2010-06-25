(ns midje.core
  (:use clojure.test)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.error-kit)
)


(def 
 #^{:doc "The best approximation to the user-written code that failed.
    In the format of clojure.test/file-position."}
  *file-position-of-call-under-test* nil)


(deferror 
  #^{:doc "Raising a failure-during-computation means 'don't check expectations'."}
  *failure-during-computation [] [])


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


(defn- find-matching-call [faked-function args expectations]
  (find-first (fn [expectation] 
		  (and (= faked-function (expectation :function))
		       (= (count args) (count (expectation :arg-matchers)))
		       (matching-args? args (expectation :arg-matchers))))
	      expectations)
)






; ======

(comment

(defn call-faker [faked-function args expectations]
  (let [found (find-matching-call faked-function args expectations)]
    (if-not found 
	    (do 
	      (report {:type :fail
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


(defn check-call-counts [expectations]
  (doseq [expectation expectations]
      (if (zero? @(expectation :count-atom))
	  (report {:type :fail 
		   :message (str "The form being tested near "
				 (position-string *file-position-of-call-under-test*)
				 " didn't make a call that was expected.")
		   :expected (str (expectation :call-text-for-failures) " should be called once.")
		   :actual "Never called"})))
)


(defmacro short-circuiting-failure 
  ([form check-type expected]
   `(short-circuiting-failure ~form ~check-type ~expected nil))

  ([form check-type expected failure-message]
  `(with-handler (let [code-under-test-result# (eagerly ~form)]
		   (is (= code-under-test-result# ~expected) ~failure-message))
		 (handle *failure-during-computation* [])))
)

(defn unique-function-symbols [expectations]
  (vec (set (map #(:function %) expectations)))
)


(defn binding-map [expectations]
  (reduce (fn [accumulator function-symbol] 
	      (let [function-var (intern *ns* function-symbol)
		    faker (fn [& actual-args] (call-faker function-symbol actual-args expectations))]
		(assoc accumulator function-var faker)))
	  {}
	  (unique-function-symbols expectations))
)


(defmacro during 
  ([call-form ignored expected-result expectations]
   `(during ~call-form ~ignored ~expected-result ~expectations {}))

  ([call-form ignored expected-result expectations annotations]
     (let [expectations-symbol (gensym "expectations")]
       `(binding [*file-position-of-call-under-test* (or ~(annotations :position) (file-position 1))]
	  (let [~expectations-symbol ~expectations]
	    (with-bindings (binding-map ~expectations-symbol)
			   (short-circuiting-failure ~call-form :equality ~expected-result))
	    (check-call-counts ~expectations-symbol)))))
)


(defmacro fake [call-form ignored resulting-result]
  `{:function '~(first call-form)
    :arg-matchers [ (fn [actual#] (= actual# ~(second call-form))) ]
    :call-text-for-failures (str '~call-form)
    :result-supplier (fn [] ~resulting-result)
    :count-atom (atom 0)
    :file-position (file-position 2)}
)

)
