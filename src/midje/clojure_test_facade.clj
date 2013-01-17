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
(defn note-pass [] (ct/inc-report-counter :pass))
(defn note-fail [] (ct/inc-report-counter :fail))
(defn note-test [] (ct/inc-report-counter :test))

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
  (with-isolated-counters
    (binding [ct/*test-out* (java.io.StringWriter.)]
      (assoc (apply ct/run-tests namespaces)
             :lines (-> ct/*test-out* .toString str/split-lines)))))

(defn output [& texts]
  (ct/with-test-out
    (dorun (map println texts))))

(def ^{:dynamic true} report ct/report)
