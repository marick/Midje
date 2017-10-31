(ns ^{:doc "Unfolding prerequisites like `(f (g 1)) => 3`"}
  midje.parsing.2-to-lexical-maps.folded-fakes
  (:require [clojure.zip :as zip]
            [midje.checking.checkers.defining :refer [checker? checker-makers]]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.parsing.util.fnref :as fnref]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.zip :refer :all]
            [midje.util.pile :as pile]
            [such.maps :as map]))

;; Note that unfolding is done after prerequisites are converted to fakes.

;; General strategy is to condense fake forms into a funcall=>metaconstant
;; mapping. These substitutions are used both to "flatten" a fake form and also
;; to generate new fakes.

(def #^:dynamic #^:private *metaconstant-counts*)

(defmacro with-fresh-generated-metaconstant-names [& forms]
  `(binding [*metaconstant-counts* (atom {})]
     ~@forms))

(defn metaconstant-for-form [[fnref & _ :as inner-form]]
  (let [swap-fn (fn [current-value fnref]
                  (assoc current-value fnref ((fnil inc 0) (current-value fnref))))
        number ((swap! *metaconstant-counts* swap-fn fnref)
                  fnref)]
    (symbol (format "...%s-value-%s..." (name (fnref/as-symbol fnref)) number))))

(defn- ^{:testable true } mockable-funcall? [x]
  (let [constructor? (fn [symbol]
                       (.endsWith (name symbol) "."))
        special-forms '[quote fn let new]
        mockable-function? (fn [fnref]
                             (not (or (some #{fnref} special-forms)
                                      (some #{fnref} checker-makers)
                                      (keyword? fnref)
                                      (constructor? (fnref/as-symbol fnref))
                                      (checker? (fnref/resolved-to-actual-var-object fnref)))))]
    (and (list? x)
      (mockable-function? (first x)))))

(defn- fake-form-funcall-arglist
  [[fake funcall => value & overrides :as _fake-form_]]
  (rest funcall))

(defn augment-substitutions [substitutions fake-form]
  (let [needed-keys (filter mockable-funcall? (fake-form-funcall-arglist fake-form))]
    ;; Note: because I like for a function's metaconstants to be
    ;; easily mappable to the original fake, I don't make one
    ;; unless I'm sure I need it.
    (into substitutions (for [needed-key needed-keys
                              :when (nil? (get substitutions needed-key))]
                          [needed-key (metaconstant-for-form needed-key)]))))

(defn folded-fake? [form]
  (and (sequential? form)
       (= `fake (first form))
       (sequential? (second form))
       (some mockable-funcall? (fake-form-funcall-arglist form))))

(defn generate-fakes [substitutions overrides]
  (for [[funcall metaconstant] substitutions]
    `(fake ~funcall midje.sweet/=> ~metaconstant ~@overrides)))

(defn flatten-fake [[fake [fun & args] & rest] substitutions]
  (let [new-args (for [a args] (get substitutions a a))]
    `(~fake (~fun ~@new-args) ~@rest)))

(defn- ^{:testable true } unfolding-step
  "This walks through a `pending` list that may contain fakes. Each element is
   copied to the `finished` list. If it is a suitable fake, its nested
   are flattened (replaced with a metaconstant). If the metaconstant was newly
   generated, the fake that describes it is added to the pending list. In that way,
   it'll in turn be processed. This allows arbitrarily deep nesting."
  [finished pending substitutions]
  (let [target (first pending)]
    (if-not (folded-fake? target)
      [(conj finished target),
       (rest pending),
       substitutions]

      (let [overrides (drop 4 target)
            augmented-substitutions (augment-substitutions substitutions target)
            flattened-target (flatten-fake target augmented-substitutions)
            generated-fakes (generate-fakes (map/key-difference augmented-substitutions substitutions) overrides)]
        [(conj finished flattened-target),
         (concat generated-fakes (rest pending)),
         augmented-substitutions]))))



(letfn [(unfold-expect-form__then__stay_put [loc]
          (loop [[finished pending substitutions] [[] (zip/node loc) {}]]
            (if (empty? pending)
              (zip/replace loc (apply list finished))
              (recur (unfolding-step finished pending substitutions)))))]

  (defn unfold-fakes [form]
    (with-fresh-generated-metaconstant-names
      (translate-zipper form
        recognize/expect?
        unfold-expect-form__then__stay_put))))
