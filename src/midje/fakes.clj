;; -*- indent-tabs-mode: nil -*-

(ns midje.fakes
  (:use [clojure.contrib.seq-utils :only [find-first]]
        clojure.test
        [midje.util report file-position form-utils exceptions
                    thread-safe-var-nesting wrapping]
        [midje.checkers :only [exactly]]
        [midje.checkers.defining :only [checker?]]
        [midje.checkers.util :only [captured-exception]]
        [midje.checkers.chatty :only [chatty-checker-falsehood? chatty-checker?]]
        [midje.checkers.extended-equality :only [extended-= extended-list-= extended-fn?]])
  (:require [clojure.zip :as zip]))

(defn common-to-all-fakes [var-sym] 
  `{:function (var ~var-sym)
    :count-atom (atom 0)
    :position (user-file-position)})

(defn make-fake-map 
  [var-sym special-to-fake-type user-override-pairs]
  (merge
   (common-to-all-fakes var-sym)
   special-to-fake-type
   (apply hash-map-duplicates-ok user-override-pairs)))

(defn unique-function-vars [fakes]
  (distinct (map #(:function %) fakes)))

(defmulti matches-call? (fn [fake faked-function args]
                          (:type fake)))

(defmethod matches-call? :not-called
  [fake faked-function args]
  (= faked-function (fake :function)))

(defmethod matches-call? :default
  [fake faked-function args]
  (and (= faked-function (fake :function))
       (= (count args) (count (fake :arg-matchers)))
       (extended-list-= args (fake :arg-matchers))))

(defn find-matching-call [faked-function args fakes]
  (find-first #(matches-call? % faked-function args) fakes))

(defn call-faker [faked-function args fakes]
  "This is the function that handles all mocked calls."
  (let [found (find-matching-call faked-function args fakes)]
    (if-not found 
      (do 
        (clojure.test/report {:type :mock-argument-match-failure
                 :function faked-function
                 :actual args
                 :position (:position (first fakes))}))
      (do 
        (swap! (found :count-atom) inc)
        ((found :result-supplier)))))
  )

(defn binding-map [fakes]
  (reduce (fn [accumulator function-var] 
              (let [faker (fn [& actual-args] (call-faker function-var actual-args fakes))]
                (assoc accumulator function-var faker)))
          {}
          (unique-function-vars fakes)))

(defn fake-count [fake] (deref (:count-atom fake)))

(defmulti call-count-incorrect? :type)

(defmethod call-count-incorrect? :fake
  [fake]
  (let [method (or (:times fake) :default)
        count (fake-count fake)]
    (cond (= method :default)
          (zero? count)

          (number? method)
          (not= method count)

          (coll? method)
          (not (some #{count} method))

          (fn? method)
          (not (method count)))))

(defmethod call-count-incorrect? :not-called
  [fake]
  (not (zero? (fake-count fake))))

(defmethod call-count-incorrect? :background
  [fake]
  false)

(defn check-call-counts [fakes]
  (doseq [fake fakes]
    (when (call-count-incorrect? fake)
      (report {:type :mock-incorrect-call-count
               :actual-count @(fake :count-atom)
               :expected-call (fake :call-text-for-failures)
               :position (:position fake)
               :expected (fake :call-text-for-failures)}))))

(defn arg-matcher-maker 
  "Based on an expected value, generates a function that returns true if the 
   actual value matches it."
  [expected]
  (if (and (extended-fn? expected)
           (not (checker? expected)))
    (fn [actual] (extended-= actual (exactly expected)))
    (fn [actual] (extended-= actual expected))))

;; Managing background fakes

(defn background-fakes []
  (namespace-values-inside-out :midje/background-fakes))

(defn background-fake-wrappers [fakes]
  (let [around-facts-and-checks `(with-pushed-namespace-values
                                   :midje/background-fakes
                                   ~fakes ~(?form))]
    (list 
     (with-wrapping-target around-facts-and-checks :facts))))

