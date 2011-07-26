;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.fakes
  (:use
    [clojure.contrib.seq :only [find-first]]
    [clojure.test :only [report]]
    [midje.checkers :only [exactly]]
    [midje.checkers.defining :only [checker?]]
    [midje.checkers.extended-equality :only [extended-= extended-list-= extended-fn?]]
    [midje.util.file-position :only [user-file-position]]
    [midje.util.form-utils :only [hash-map-duplicates-ok]]
    [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out 
                                               with-pushed-namespace-values]]
    [midje.internal-ideas.file-position :only [arrow-line-number-from-form]]
    [midje.internal-ideas.wrapping :only [with-wrapping-target]]
    [midje.util.form-utils :only [form-first?]]))

(defn tag-function-as-fake [function]
  (vary-meta function assoc :midje/faked-function true))

(defn function-tagged-as-fake? [function]
  (:midje/faked-function (meta function)))

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
            (let [faker (fn [& actual-args] (call-faker function-var actual-args fakes))
                  tagged-faker (tag-function-as-fake faker)]
                (assoc accumulator function-var tagged-faker)))
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

;;

(defn make-fake [fake-body]
  (let [line-number (arrow-line-number-from-form fake-body)]
    (vary-meta
     `(midje.semi-sweet/fake ~@fake-body)
     assoc :line line-number)))

(defn tag-as-background-fake [fake]
  (concat fake '(:type :background)))

(defn fake? [form] (form-first? form "fake"))

(defn fake-form-funcall [fake-form]
  (second fake-form))

(defn fake-form-funcall-arglist [fake-form]
  (rest (fake-form-funcall fake-form)))

