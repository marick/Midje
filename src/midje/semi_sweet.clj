(ns midje.semi-sweet
  (:use clojure.test
	midje.semi-sweet.semi-sweet-internals
        [clojure.contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.unprocessed)

(def => "=>")   ; So every namespace uses the same qualified name.

(defonce
  #^{:doc "True by default.  If set to false, Midje checks are not
     included into production code, whether compiled or loaded."}
  *include-midje-checks* true)

(defmacro only-mocked 
  "Defines a list of names as functions that have no implementation yet. They will
   throw Errors if ever called."
  [& names] (only-mocked* names))

(defmacro unfinished
  "Defines a list of names as functions that have no implementation yet. They will
   throw Errors if ever called."
  [& names] (only-mocked* names))

(defmacro fake 
  "Creates an fake map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  [call-form => result & overrides]
  (let [[var-sym & args] call-form & overrides]
    ;; The (vec args) keeps something like (...o...) from being evaluated as a
    ;; function call later on. Right approach would seem to be '~args. That causes
    ;; spurious failures. Debug someday.
    (make-fake-map var-sym
                          `{:arg-matchers (map midje.fakes/arg-matcher-maker ~(vec args))
                            :call-text-for-failures (str '~call-form)
                            :result-supplier (fn [] ~result)
			    :type :fake}
			  overrides))
)

(defmacro not-called
  "Creates an fake map that a function will not be called.
   Example: (not-called f))"
  [var-sym & overrides]
  (make-fake-map var-sym
                        `{:call-text-for-failures (str '~var-sym " was called.")
                          :result-supplier (fn [] nil)
                          :type :not-called}
			overrides)
)

(defn- value-within [namespace-symbol variable-symbol]
  (let [namespace (find-ns namespace-symbol)]
    (if namespace
      (var-get ((ns-map namespace) variable-symbol))
      true)))

(defn user-desires-checking? []
  (and (value-within 'clojure.test '*load-tests*)
       (value-within 'midje.sweet '*include-midje-checks*)
       (value-within 'midje.semi-sweet '*include-midje-checks*)))

(defmacro expect 
  "Run the call form, check that all the mocks defined in the fakes 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument.

   To strip tests from production code, set either clojure.test/*load-tests*
   or midje.semi-sweet/*check* to false."
  [call-form => expected-result & overrides-and-fakes]
  (when (user-desires-checking?)
    (let [ [overrides fakes] (separate overrides-and-fakes)]
      `(let [call# (call-being-tested ~call-form ~expected-result ~overrides)]
	 (expect* call# (vector ~@fakes))))))
