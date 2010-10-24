(ns leiningen.midje
  (:use [leiningen.util.ns :only [namespaces-in-dir]]
        [leiningen.compile :only [eval-in-project]]))

(defn require-namespaces-form [namespaces]
  `(do
     (require 'clojure.test)
     (alter-var-root (var clojure.test/*report-counters*)
		     (fn [_#] (ref clojure.test/*initial-report-counters*)))
     (doseq [n# '~namespaces] (require n#))
     (let [passes# (:pass @clojure.test/*report-counters*)
	   fails# (:fail @clojure.test/*report-counters*)
           failure-message# (condp = fails#
			       0 (format "All claimed facts (%d) have been confirmed." passes#)
			       1 (format "FAILURE: %d fact was not confirmed." fails#)
			       (format "FAILURE: %d facts were not confirmed." fails#))
	   potential-consolation# (condp = passes#
				      0 ""
				      1 "(But 1 was.)"
				      (format "(But %d were.)" passes#))
	   consolation# (if (> fails# 0) potential-consolation# "")]
       (println failure-message# consolation#))))

(defn midje [project & namespaces]
  (let [desired-namespaces  (if (empty? namespaces)
			      (concat (namespaces-in-dir (:test-path project))
				      (namespaces-in-dir (:source-path project)))
			      (map symbol namespaces))]
    (eval-in-project project
		     (require-namespaces-form desired-namespaces))))
