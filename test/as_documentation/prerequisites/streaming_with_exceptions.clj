(ns as-documentation.prerequisites.streaming-with-exceptions
  (:require [midje.sweet :refer :all]))

;; By treating the correctness of functions as facts depending on
;; prerequisites, we can test handling of a stream of values that
;; includes exceptions without a *whole* lot of fuss. This style will
;; be explained more fully in https://leanpub.com/utpov by Fields,
;; Feathers, and Marick.

;; 1: Turn function calls into data.

;; See chapter 12 of https://leanpub.com/fp-oo for the idea behind
;; this function.
(defn value-stream
  "Convert on-demand calls of a function into a lazy seq of values.
   Note that the stream is assumed to be infinite - there is no
   'out of values' indicator."
  [stream-fn]
  (cons (stream-fn) (lazy-seq (value-stream stream-fn))))

(def source-of-values-fn)

(fact "`value-stream` converts a function into a lazy seq of values"
  (take 3 (value-stream (constantly 1))) => [1 1 1]

  (fact "works with Midje's =streams=>"
    (take 3 (value-stream #'source-of-values-fn)) => [1 2 3]
    (provided
      (source-of-values-fn) =streams=> [1 2 3])))


;; 2: Independently, handle exceptions.

(defn value-or-sentinel
  "Return value of a function, except in the case of an exception.
   For exception, return the value `sentinel-function`, which is
   given the exception"
  [f sentinel-fn]
     (try (f)
     (catch Exception ex
       (sentinel-fn ex))))

(fact "`value-or-sentinel returns a sentinel value upon exception"
  (value-or-sentinel (constantly 2) ..unused..) => 2

  (value-or-sentinel #'source-of-values-fn (constantly ::sentinel)) => ::sentinel
  (provided
    (source-of-values-fn) =throws=> (new Exception))

  (value-or-sentinel #'source-of-values-fn #(.getMessage %)) => "hi"
  (provided
    (source-of-values-fn) =throws=> (new Exception "hi")))


;; 3: Compose the two ideas.

(defn sentinelized-stream
  "Convert stream into a lazy sequence of values, with
   ::sentinel replacing any exceptions."
  [stream-fn]
  (value-stream #(value-or-sentinel stream-fn (constantly ::sentinel))))

(facts "about sentinelized streams"
  (fact "no exceptions => no problem"
    (take 3 (sentinelized-stream source-of-values-fn)) => [1 2 3]
    (provided
     (source-of-values-fn) =streams=> [1 2 3]))

  (fact "exceptions are converted into sentinel"
    (take 2 (sentinelized-stream source-of-values-fn)) => [::sentinel ::sentinel]
    (provided
      (source-of-values-fn) =throws=> (new Exception))))

;; Per J.B. Rainsberger, "test until fear turns to boredom". I'm confident that the above
;; will work for a mixture of real values and exceptions, but let's see!
;;
;; Note: my first thought was to construct a lazy stream like this:
;;
;;   (source-of-values-fn) =streams=> (cons 1
;;                                          (lazy-seq (cons (throw (new Exception "boom!"))
;;                                                          [3])))))
;;
;; That doesn't work - when the `throw happens`, the `cons` is bypassed as the exception goes
;; up the stack. Thereafter, the next value is always an exception because the cached sequence
;; never changes. So we have to resort to explicit state.

(defn mkfn:fail-on [fail-set]
  (let [count (atom 0)]
    (fn []
      (let [retval (swap! count inc)]
        (if (fail-set retval)
          (throw (new Exception))
          retval)))))

(def fail-on-first (mkfn:fail-on #{1}))
(def fail-on-second (mkfn:fail-on #{2}))
(def fail-on-third (mkfn:fail-on #{3}))
(def fail-on-bounds (mkfn:fail-on #{1 3}))


(facts "end-to-end-like"
  (take 3 (sentinelized-stream fail-on-first)) => [::sentinel 2 3]
  (take 3 (sentinelized-stream fail-on-second)) => [1 ::sentinel 3]
  (take 3 (sentinelized-stream fail-on-third)) => [1 2 ::sentinel]
  (take 3 (sentinelized-stream fail-on-bounds)) => [::sentinel 2 ::sentinel])

