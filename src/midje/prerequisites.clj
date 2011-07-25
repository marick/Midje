(ns midje.prerequisites
  (:use [midje.util.namespace :only [namespacey-match]]
        [midje.checkers.defining :only [checker-makers checker?]]
        [midje.fakes :only [fake-form-funcall-arglist]]
  ))

(defn is-head-of-form-providing-prerequisites? [loc]
  (namespacey-match '(provided) loc))


;; Folded prerequisites

;; Note that folded prerequisites are in semi-sweet-style. (That is, they can only
;; be recognized after sweet style has been converted to semi-sweet.)

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

(defn folded-prerequisite? [form]
  (and (sequential? form)
       (= 'midje.semi-sweet/fake (first form))
       ;; We now know this: (fake (f ...arg... ...arg...) ...)
       (some mockable-funcall? (fake-form-funcall-arglist form))))

