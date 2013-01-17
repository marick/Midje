(ns ^{:doc "Midje makes use of clojure.test for its reporting. This will
            (eventually) gather all the dependencies here."}
  midje.clojure-test-facade
  (:require [clojure.test :as ct]
            [clojure.string :as str]))

;; This turns off "Testing ...." lines, which I hate, especially
;; when there's no failure output. The type check is because
;; `lein test` overrides clojure.test/report with a non-multimethod.
(when (= clojure.lang.MultiFn (type clojure.test/report))
  (defmethod clojure.test/report :begin-test-ns [m]))

;; The only use for clojure.test counters is so `lein-test` works (haltingly) with Midje.
(defn note-pass [] (ct/inc-report-counter :pass))
(defn note-fail [] (ct/inc-report-counter :fail))
(defn note-test [] (ct/inc-report-counter :test))


(defn run-tests
  "Run clojure.test tests in the given namespaces. It does not 
   affect the Midje fact counters but instead returns a map
   that can be used to produce a separate report."
  [namespaces]
  (binding [ct/*test-out* (java.io.StringWriter.)]
    (assoc (apply ct/run-tests namespaces)
           :lines (-> ct/*test-out* .toString str/split-lines))))

(defn forget-failures
  "This can only be used within the dynamic scope of run-tests."
  []
  (dosync (commute ct/*report-counters* assoc :fail 0)))

(defn output [& texts]
  (ct/with-test-out
    (dorun (map println texts))))

(def ^{:dynamic true} report ct/report)
