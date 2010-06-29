(ns midje.semi-sweet
  (:use clojure.test)
  (:use midje.unprocessed)
  (:use midje.checkers)
  (:use midje.report)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.error-kit)
)


(defn- only-mocked* [names]
  (let [declarations (map (fn [name] 
			      `(defn ~name [& args#] 
				 (throw (Error. (str '~name " has no implementation. It's used in mock tests.")))))
			  names)]
    `(do ~@declarations)))

(defmacro only-mocked 
  "Defines a list of names as functions that have no implementation yet. They will
   throw Errors if ever called."
  [& names] (only-mocked* names))

(defmacro fake 
  "Creates an expectation map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  [call-form => result]
  `{:function (def ~(first call-form))
    :arg-matchers (map arg-matcher-maker [~@(rest call-form)])
    :call-text-for-failures (str '~call-form)
    :result-supplier (fn [] ~result)
    :count-atom (atom 0)
    :file-position (user-file-position)}
)


(defmacro call-being-tested [call-form expected-result]
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect'."
   `{:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :expected-result-text-for-failures '~expected-result
     :file-position (user-file-position)})

(defmacro expect 
  "Run the call form, check that all the mocks defined in the expectations 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument."
  [call-form => expected-result & expectations]
   `(let [call# (call-being-tested ~call-form ~expected-result)]
      (expect* call# (vector ~@expectations)))
)
