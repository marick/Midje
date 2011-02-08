;; -*- indent-tabs-mode: nil -*-

(ns midje.error-handling
  (:use [clojure.contrib.pprint :only [cl-format]]
        [midje.util report file-position form-utils]
        [clojure.test]))

(defn make-broken-fake [overrides & message-args]
  (merge `{:type :broken-fake
           :message ~(apply (partial cl-format nil) message-args)
          :position (user-file-position) }
         (apply hash-map-duplicates-ok overrides)))

(defn broken-fake? [thing]
  (and (map? thing) (= (:type thing) :broken-fake)))

(defn report-broken-fakes [brokens]
  (doseq [busted brokens]
    (report {:type :user-error
             :message (:message busted)
             :position (:position busted)})))

(defn best-guess-at-overrides [fake-forms]
  (let [ [_ _ _ & overrides] fake-forms] overrides))

(defn broken-fake [forms]
  (cond (not (list? (first forms)))
        (make-broken-fake
         (best-guess-at-overrides forms)
         "Left-hand-side must look like a function call. ~S doesn't."
         (first forms))

        :else
        nil))
    
