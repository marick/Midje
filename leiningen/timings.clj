;; -*- indent-tabs-mode: nil -*-

(ns leiningen.timings
  (:refer-clojure :exclude [test])
  (:use [leiningen.util.ns :only [namespaces-in-dir]]
        [leiningen.test :only [*exit-after-tests*]]
        [leiningen.compile :only [eval-in-project]]
        [clojure.set :only [difference]]))

(defn require-namespaces-form [namespaces]
  `(let [array# (doall (map (fn [_#] (let [start# (.getTime (java.util.Date.))]
                                (when (= clojure.lang.MultiFn (type clojure.test/report))
                                  (defmethod clojure.test/report :begin-test-ns [m#]))

                                (alter-var-root (var clojure.test/*report-counters*)
                                                (fn [_#] (ref clojure.test/*initial-report-counters*)))

                                (dosync (alter @#'clojure.core/*loaded-libs* difference (set '~namespaces)))
                                (doseq [n# '~namespaces] (require n# :reload))

                                (- (.getTime (java.util.Date.)) start#)))
                     (range 7)))]
     (println (sort array#))))

(defn timings 
  "Run both Midje and clojure.test tests.
   Namespaces are looked up in both the src/ and test/ subdirectories.
   If no namespaces are given, runs tests in all namespaces."
  [project & namespaces]
  (let [desired-namespaces  (if (empty? namespaces)
                              (concat (namespaces-in-dir (:test-path project))
                                      (namespaces-in-dir (:source-path project)))
                              (map symbol namespaces))]
    (eval-in-project project
                     (require-namespaces-form desired-namespaces)
                     nil
                     nil
                     '(require '[clojure walk template stacktrace test string set]))))
                     
