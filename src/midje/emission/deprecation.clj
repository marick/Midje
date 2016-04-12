(ns ^{:doc "Utilities to deprecate features."}
  midje.emission.deprecation
  (:require [midje.config :as config]
            [midje.emission.util :refer :all]
            [pointer.core :refer [compile-time-fallback-position]]))

(def any-deprecations? (atom :uninitialized))
(def deprecation-record (atom :uninitialized))
(defn initialize []
  (reset! any-deprecations? false)
  (reset! deprecation-record #{}))
(initialize)

(defn show-all? []
  (= (config/choice :visible-deprecation) :all))


(defn- note-first-deprecation []
  (when (and (not @any-deprecations?)
             (not (show-all?)))
    (println "====")
    (println "== You are using deprecated features. If you'd like to see")
    (println "== all uses, including the filename and (rough) line number,")
    (println "== set configuration variable :visible-deprecation to :all.")
    (println "====")
    (swap! any-deprecations? (constantly true))))

(defn deprecate [message]
  (when (config/choice :visible-deprecation)
    (note-first-deprecation)
    (when (or (show-all?)
              (not (@deprecation-record message)))
      (swap! deprecation-record conj message)
      (println message (midje-position-string (compile-time-fallback-position))))))

(defmacro without-previous-deprecations [& body]
  `(let [prev-any-deprecations?# @any-deprecations?
         prev-deprecation-record# @deprecation-record]
     (try
       (initialize)
       ~@body
     (finally
      (reset! any-deprecations? prev-any-deprecations?#)
      (reset! deprecation-record prev-deprecation-record#)))))
