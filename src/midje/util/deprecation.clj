(ns ^{:doc "Utilities to deprecate features."}
  midje.util.deprecation
  (:use [midje.internal-ideas.file-position :only [compile-time-fallback-position]]
        [midje.ideas.reporting.string-format :only [midje-position-string]]
        [midje.util.ecosystem :only [getenv]]))

(def any-deprecations? (atom false))
(def deprecation-record (atom #{}))

(defn- note-first-deprecation []
  (when (and (not @any-deprecations?)
             (not (getenv "MIDJE_ALL_DEPRECATIONS")))
    (println "====")
    (println "== You are using deprecated features. If you'd like to see")
    (println "== all uses, including the filename and (rough) line number,")
    (println "== set environment variable MIDJE_ALL_DEPRECATIONS to some value.")
    (println "====")
    (swap! any-deprecations? (constantly true))))

(defn deprecate [message]
  (note-first-deprecation)
  (when (or (getenv "MIDJE_ALL_DEPRECATIONS")
            (not (@deprecation-record message)))
    (swap! deprecation-record conj message)
    (println message (midje-position-string (compile-time-fallback-position)))))
