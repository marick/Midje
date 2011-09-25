;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.fakes
  (:use
    [clojure.contrib.seq :only [find-first separate]]
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
                                               with-pushed-namespace-values
                                               with-altered-roots]]
    [midje.util.exceptions :only [user-error]]
    [midje.internal-ideas.wrapping :only [with-wrapping-target]]
    [midje.ideas.arrow-symbols])
  (:require [clojure.zip :as zip])
  (:import midje.ideas.metaconstants.Metaconstant))

;;; Questions to ask of fakes // accessors

(defn implements-a-fake? [function] (:midje/faked-function (meta function)))
(defn function-fake? [form] (form-first? form "fake"))
(defn data-fake? [form] (form-first? form "data-fake"))
(defn fake? [form] (or (function-fake? form) (data-fake? form)))
(defn fake-form-funcall [[fake funcall => value & overrides]] funcall)
(defn fake-form-funcall-arglist [fake-form] (rest (fake-form-funcall fake-form)))

;;; Creation

(defn fn-that-implements-a-fake [function]
  (vary-meta function assoc :midje/faked-function true))

(defn common-to-all-fakes [var-sym] 
  `{:lhs (var ~var-sym)
    :count-atom (atom 0)
    :position (user-file-position)})

(defn make-fake-map 
  [var-sym special-to-fake-type user-override-pairs]
  (merge
   (common-to-all-fakes var-sym)
   special-to-fake-type
   (apply hash-map-duplicates-ok user-override-pairs)))

(defn arg-matcher-maker 
  "Based on an expected value, generates a function that returns true if the 
   actual value matches it."
  [expected]
  (if (and (extended-fn? expected)
           (not (checker? expected)))
    (fn [actual] (extended-= actual (exactly expected)))
    (fn [actual] (extended-= actual expected))))

(defmulti make-result-supplier (fn [arrow & _]  arrow))

(defmethod make-result-supplier => [arrow result] #(identity result))

(defmethod make-result-supplier =streams=> [arrow result-stream]
           (let [current-stream (atom result-stream)]
             #(let [current-result (first @current-stream)]
                (swap! current-stream rest)
                current-result)))

(defmethod make-result-supplier :default [arrow result-stream]
  (throw (user-error (str "It's likely you misparenthesized your metaconstant prerequisite."))))

(defn fake* [ [[var-sym & args :as call-form] arrow result & overrides] ]
  ;; The (vec args) keeps something like (...o...) from being
  ;; evaluated as a function call later on. Right approach would
  ;; seem to be '~args. That causes spurious failures. Debug
  ;; someday.
  (make-fake-map var-sym
                 `{:arg-matchers (map midje.internal-ideas.fakes/arg-matcher-maker ~(vec args))
                   :call-text-for-failures (str '~call-form)
                   :result-supplier (make-result-supplier ~arrow ~result)
                   :type :fake}
                 overrides))

(defn data-fake* [ [metaconstant arrow contained & overrides] ]
  (make-fake-map metaconstant
                 `{:contained ~contained
                  :count-atom (atom 1)  ;; CLUDKJE!
                  :type :fake
                  :data-fake :data-fake}
                 overrides))

(defn tag-as-background-fake [fake]
  (concat fake
          '(:background :background)
          '(:times (range 0))))

;;; Binding

(defmulti matches-call? (fn [fake faked-function args]
                          (:type fake)))

(defmethod matches-call? :not-called
  [fake faked-function args]
  (= faked-function (fake :lhs)))

(defmethod matches-call? :default
  [fake faked-function args]
  (and (= faked-function (fake :lhs))
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
                 :lhs faked-function
                 :actual args
                 :position (:position (first fakes))}))
      (do 
        (swap! (found :count-atom) inc)
        ((found :result-supplier)))))
  )


(defn unique-vars [fakes]
  (distinct (map :lhs fakes)))


(defn binding-map-with-function-fakes [fakes]
  (reduce (fn [accumulator var] 
            (let [faker (fn [& actual-args] (call-faker var actual-args fakes))
                  tagged-faker (fn-that-implements-a-fake faker)]
                (assoc accumulator var tagged-faker)))
          {}
          (unique-vars fakes)))


(defn data-fakes-to-metaconstant-bindings [fakes]
  (map (fn [{var :lhs, contents :contained}]
         (let [name (:name (meta var))]
           {var (Metaconstant. name contents)}))
       fakes))

(defn merge-metaconstant-bindings [bindings]
  (apply merge-with (fn [v1 v2]
                      (Metaconstant. (.name v1) (merge (.storage v1) (.storage v2))))
         bindings))

  
(defn binding-map-with-data-fakes [fakes]
  (merge-metaconstant-bindings (data-fakes-to-metaconstant-bindings fakes)))

(defn binding-map [fakes]
  (let [[data-fakes function-fakes] (separate :data-fake fakes)]
    (merge (binding-map-with-function-fakes function-fakes)
           (binding-map-with-data-fakes data-fakes)
           )))

(defmacro with-installed-fakes [fakes & forms]
  `(with-altered-roots (binding-map ~fakes) ~@forms))

;;; Checking

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

(defn check-call-counts [fakes]
  (doseq [fake fakes]
    (when (call-count-incorrect? fake)
      (report {:type :mock-incorrect-call-count
               :actual-count @(fake :count-atom)
               :expected-call (fake :call-text-for-failures)
               :position (:position fake)
               :expected (fake :call-text-for-failures)}))))



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

;; This walks through a `pending` list that may contain fakes. Each element is
;; copied to the `finished` list. If it is a suitable fake, its nested 
;; are flattened (replaced with a metaconstant). If the metaconstant was newly
;; generated, the fake that describes it is added to the pending list. In that way,
;; it'll in turn be processed. This allows arbitrarily deep nesting.

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

