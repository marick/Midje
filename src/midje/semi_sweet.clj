(ns midje.semi-sweet
  (:use clojure.test)
  (:use midje.unprocessed)
  (:use midje.report)
  (:use clojure.contrib.seq-utils)
  (:use clojure.contrib.error-kit)
)

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


(defmacro call-being-tested [call-form expected-result]
   `{:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :file-position (user-file-position)})

(defmacro expect 
  "doc string here"
  ([call-form => expected-result]
   `(expect ~call-form => ~expected-result []))
  ([call-form => expected-result expectations]
   `(let [call# (call-being-tested ~call-form ~expected-result)]
      (expect* call# ~expectations)))
)
