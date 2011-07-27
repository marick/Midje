;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.fakes
  (:use
    [clojure.contrib.seq :only [find-first]]
    [clojure.test :only [report]]
    [midje.checkers :only [exactly]]
    [midje.checkers.defining :only [checker? checker-makers]]
    [midje.internal-ideas.expect :only [expect? up-to-full-expect-form]]
    [midje.util.form-utils :only [form-first?
                                  translate
                                  map-difference
                                  hash-map-duplicates-ok]]
    [midje.ideas.metaconstants :only [metaconstant-for-form
                                      with-fresh-generated-metaconstant-names]]
    [midje.checkers.extended-equality :only [extended-= extended-list-= extended-fn?]]
    [midje.internal-ideas.file-position :only [user-file-position]]
    [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out 
                                               with-pushed-namespace-values]]
    [midje.internal-ideas.file-position :only [arrow-line-number-from-form]]
    [midje.internal-ideas.wrapping :only [with-wrapping-target]])
  (:require [clojure.zip :as zip]))

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




;; This walks through a `pending` list that may contain fakes. Each element is
;; copied to the `finished` list. If it is a suitable fake, its nested 
;; are flattened (replaced with a metaconstant). If the metaconstant was newly
;; generated, the fake that describes it is added to the pending list. In that way,
;; it'll in turn be processed. This allows arbitrarily deep nesting.

;; Folded prerequisites

;; Note that folded prerequisites are in semi-sweet-style. (That is, they can only
;; be recognized after sweet style has been converted to semi-sweet.)


;; General strategy is to condense fake forms into a funcall=>metaconstant
;; mapping. These substitutions are used both to "flatten" a fake form and also
;; to generate new fakes.

(def special-forms '[quote fn let new])

(defn- constructor? [symbol]
  (.endsWith (name symbol) "."))

(defn- mockable-function-symbol? [symbol]
  (not (or (some #{symbol} special-forms)
           (some #{symbol} checker-makers)
           (constructor? symbol)
           (checker? (resolve symbol)))))



(defn mockable-funcall? [thing]
  (and (list? thing)
       (mockable-function-symbol? (first thing))))


(defn folded-fake? [form]
  (and (sequential? form)
       (= 'midje.semi-sweet/fake (first form))
       ;; We now know this: (fake (f ...arg... ...arg...) ...)
       (some mockable-funcall? (fake-form-funcall-arglist form))))

(defn augment-substitutions [substitutions fake-form]
  (let [needed-keys (filter mockable-funcall?
                            (fake-form-funcall-arglist fake-form))]
    (reduce (fn [substitutions needed-key]
              ;; Note: because I like for a function's metaconstants to be
              ;; easily mappable to the original fake, I don't make one
              ;; unless I'm sure I need it.
              (if (get substitutions needed-key)
                substitutions
                (assoc substitutions needed-key (metaconstant-for-form needed-key))))
            substitutions
            needed-keys)))

(defn flatten-fake [ [fake [fun & args] & rest] substitutions]
  (let [new-args (map (fn [arg] (get substitutions arg arg)) args)]
    `(~fake (~fun ~@new-args) ~@rest)))

(defn generate-fakes [substitutions overrides]
  (map (fn [ [funcall metaconstant] ]
         `(midje.semi-sweet/fake ~funcall midje.semi-sweet/=> ~metaconstant ~@overrides))
       substitutions))

(defn unfolding-step [finished pending substitutions]
  (let [target (first pending)]
    (if (folded-fake? target)
      (let [overrides (nthnext target 4)
            augmented-substitutions (augment-substitutions substitutions target)
            flattened-target (flatten-fake target augmented-substitutions)
            generated-fakes (generate-fakes
                             (map-difference augmented-substitutions substitutions)
                             overrides)]
        [ (conj finished flattened-target)
          (concat generated-fakes (rest pending))
          augmented-substitutions])
    [(conj finished target), (rest pending), substitutions])))
  
(defn unfold-expect-form__then__stay_put [loc]
  (loop [ [finished pending substitutions] [ [] (zip/node loc) {} ]]
    (if (empty? pending)
      (zip/replace loc (apply list finished))
      (recur (unfolding-step finished pending substitutions)))))

(defn unfold-fakes [form]
  (with-fresh-generated-metaconstant-names
    (translate form
        expect?
        unfold-expect-form__then__stay_put)))

