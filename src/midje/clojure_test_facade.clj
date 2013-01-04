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

(defn set-counters [map]
  (alter-var-root #'ct/*report-counters*
                  (constantly (ref map))))

(defn zero-counters []
  (set-counters ct/*initial-report-counters*))
(zero-counters)

(defn counters []  @ct/*report-counters*)

(defn note-pass []
  (set-counters (merge-with + (counters) {:pass 1})))

(defn note-fail []
  (set-counters (merge-with + (counters) {:fail 1})))


(defmacro ignoring-counter-changes [& forms]
  `(let [stashed-counters# (counters)]
    (try 
      ~@forms
    (finally
       (set-counters stashed-counters#)))))

(defn reset-counters [counters]
  (alter-var-root (var ct/*report-counters*)
                  (constantly counters)))

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
  (println "=================== Need to check if failing clojure.test and embedded fact output appear in the right section of the summary.")
  (with-isolated-counters
    (binding [ct/*test-out* (java.io.StringWriter.)]
      (let [ct-result (apply ct/run-tests namespaces)]
        {:test-count (:test ct-result)
         :fail-count (+ (:fail ct-result) (:error ct-result))
         :lines (-> ct/*test-out* .toString str/split-lines)}))))

(def ^{:dynamic true} report ct/report)
