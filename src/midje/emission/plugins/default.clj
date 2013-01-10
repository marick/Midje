(ns ^{:doc "The default for Midje output"}
  midje.emission.plugins.default
  (:use [midje.util.form-utils :only [midje-position-string]])
  (:require [midje.clojure-test-facade :as ctf]
            [midje.util.colorize :as color]
            [midje.emission.state :as state]
            [midje.ideas.metadata :as metadata]
            [midje.emission.plugins.silence :as silence]))

(defn fail [report-map]
  (clojure.test/report report-map))


(def last-namespace-shown (atom nil))

(defn set-last-namespace-shown! [string]
  (reset! last-namespace-shown string))

(defn possible-new-namespace [namespace-symbol]
  (when (not= namespace-symbol @last-namespace-shown)
    (println (color/note (str "= Namespace " namespace-symbol)))
    (set-last-namespace-shown! namespace-symbol)))

(defn forget-everything []
  (set-last-namespace-shown! nil))

(defn starting-to-check-fact [fact-function]
  (ctf/output (color/note
               (str "Checking "
                    (or (metadata/fact-name fact-function)
                        (metadata/fact-description fact-function)
                        (str "fact at " (midje-position-string
                                         [(metadata/fact-file fact-function)
                                          (metadata/fact-line fact-function)])))))))


(defn make-map [& keys]
  (zipmap keys
          (map #(ns-resolve *ns* (symbol (name %))) keys)))


(def emission-map (merge silence/emission-map
                         (make-map ;; `:pass` takes no arguments. In this case,
                                   ;; it's defined in the silence plugin.
                                   ;; Note that the count of passes and fails
                                   ;; is maintained in `state`, so you needn't
                                   ;; do it yourself.
                                   ;; :pass

                                   ;; `:fail` takes a map of information. See
                                   ;; `default-fails` for the different sorts
                                   ;; of maps and how they're handled.
                                   ;; Note that the count of passes and fails
                                   ;; is maintained in `state`, so you needn't
                                   ;; do it yourself.
                                   :fail

                                   ;; `:starting-to-check-fact` takes a fact-function
                                   ;; as an argument. Use its metadata to construct
                                   ;; output. See the function above for an example.
                                   :starting-to-check-fact

                                   ;; `possible-new-namespace` is called whenever a
                                   ;; namespace might change, most notably whenever
                                   ;; a new fact is being checked.
                                   :possible-new-namespace

                                   ;; `:forget-everything` is called when a new
                                   ;; "stream" of facts is to be evaluated. The most
                                   ;; typical case is when `load-facts` is called,
                                   ;; either directly or via `lein midje`. Note that
                                   ;; the running count of passes and fails is zeroed
                                   ;; before this call.
                                   :forget-everything)))
  
(state/install-emission-map emission-map)
