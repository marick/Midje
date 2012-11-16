(ns ^{:doc "Midje makes use of clojure.test for its reporting. This will
            (eventually) gather all the dependencies here."}
  midje.clojure-test-facade
  (:require [clojure.test :as ct]
            [clojure.string :as str]))

;; Note: `clojure.test/report` will probably be the last thing to
;; come. When that happens, note that you must define `report` like this:
;;   (defn report [map] (ct/report map))
;; ... rather than
;;   (def report ct/report)
;; That's because `report` is a multimethod.

;; Note that I'm not convinced that using clojure.test counters for
;; Midje is really useful any more. The only reason for it that I can
;; think of is so that `lein test` works with Midje. 

(defn zero-counters []
  (alter-var-root (var ct/*report-counters*)
                  (fn [_#] (ref ct/*initial-report-counters*))))

(defn counters []  @ct/*report-counters*)

(defmacro with-isolated-counters [& body]
  `(let [original-value# ct/*report-counters*]
     (try
       (zero-counters)
       ~@body
     (finally
      (alter-var-root #'ct/*report-counters* (constantly original-value#))))))

(defn run-tests
  "Run clojure.test tests in the given namespaces. It does not 
   affect the Midje fact counters but instead returns a map
   that can be used to produce a separate report."
  [namespaces]
  (with-isolated-counters
    (binding [ct/*test-out* (java.io.StringWriter.)]
      (let [ct-result (apply ct/run-tests namespaces)]
        {:test-count (:test ct-result)
         :fail-count (+ (:fail ct-result) (:error ct-result))
         :lines (-> ct/*test-out* .toString str/split-lines)}))))
