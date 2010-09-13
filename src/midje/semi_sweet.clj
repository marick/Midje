(ns midje.semi-sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]))

(immigrate 'midje.unprocessed)
(if (re-find #"1.1" (clojure-version))
  (use '[clojure.contrib.seq-utils :only [group-by]]))

(def => "=>")   ; So every namespace uses the same qualified name.

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

(defn- common-to-all-expectations [var-sym] 
  `{:function (var ~var-sym)
    :count-atom (atom 0)
    :file-position (user-file-position)})

;; In case of multiple keys, the last one takes precedence.
(defn midje-override-map [keys-and-vals]
  (if (empty? keys-and-vals)
    {}
    (apply assoc (cons {} keys-and-vals))))

(defn- make-expectation-map 
  [var-sym special-to-expectation-type user-override-pairs]
  `(do
     ~(when-not (resolve var-sym) `(def ~var-sym))
     ~(merge
       (common-to-all-expectations var-sym)
       special-to-expectation-type
       (midje-override-map user-override-pairs)))
)

(defmacro fake 
  "Creates an expectation map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  [call-form => result & overrides]
  (let [[var-sym & args] call-form & overrides]
    (make-expectation-map var-sym
                          `{:arg-matchers (map arg-matcher-maker [~@args])
                            :call-text-for-failures (str '~call-form)
                            :result-supplier (fn [] ~result)
			    :type :fake}
			  overrides))
)

(defmacro not-called
  "Creates an expectation map that a function will not be called.
   Example: (not-called f))"
  [var-sym & overrides]
  (make-expectation-map var-sym
                        `{:call-text-for-failures (str '~var-sym " was called.")
                          :result-supplier (fn [] nil)
                          :type :not-called}
			overrides)
)


(defmacro call-being-tested [call-form expected-result overrides]
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect'."
  `(merge
    {:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :expected-result-text-for-failures '~expected-result
     :file-position (user-file-position)}
    (midje-override-map ~overrides)))

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.


(defn- separate [overrides-and-expectations]
  (let [expectation? #(and (seq? %)
			   (or (= "fake" (name (first %)))
			       (= "not-called" (name (first %)))))
	grouped (group-by expectation? overrides-and-expectations)
	default-values {false '() true '()}
	separated (merge default-values grouped)]
    [(separated false) (separated true)])
)

(defmacro expect 
  "Run the call form, check that all the mocks defined in the expectations 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument."
  [call-form => expected-result & overrides-and-expectations]
  (let [ [overrides expectations] (separate overrides-and-expectations)]
    `(let [call# (call-being-tested ~call-form ~expected-result ~overrides)]
       (expect* call# (vector ~@expectations))))
)
