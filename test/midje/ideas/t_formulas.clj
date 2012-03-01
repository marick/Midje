;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-formulas
  (:use midje.test-util
        midje.sweet))


;; Validation

(causes-validation-error #"There is no arrow in your formula form"
  (formula [a 1]))

(causes-validation-error #"There is no arrow in your formula form"
  (formula [a 1] 1))

(causes-validation-error #"There is no arrow in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(causes-validation-error #"There is no arrow in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" :not-vector 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [a 1 1] 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [] 1 => 1))

(def ^{:dynamic true} 
  *rnd*
  (java.util.Random. 42))

(defn uniform
  "Uniform distribution from lo (inclusive) to high (exclusive).
   Defaults to range of Java long."
  (^long [] (.nextLong *rnd*))
  (^long[lo hi] {:pre [(< lo hi)]}
    (clojure.core/long (Math/floor (+ lo (* (.nextDouble *rnd*) (- hi lo)))))))


(defn- gen-int
  ([]
    (uniform Integer/MIN_VALUE Integer/MAX_VALUE))
  ([pred] 
    (first (filter pred (repeatedly gen-int)))))

(formula [n (gen-int #(< % 2))]
  (binding [midje.ideas.formulas/*num-generations-per-formula* n] nil) 
     => (throws #"Must be an integer greater than 1."))

(formula [n (gen-int #(>= % 2))]
  (binding [midje.ideas.formulas/*num-generations-per-formula* n] nil) 
     =not=> (throws Exception))