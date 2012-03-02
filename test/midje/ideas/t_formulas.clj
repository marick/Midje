;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-formulas
  (:use midje.test-util
        midje.sweet))


;;;; Validation

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

(def ^{:private true :dynamic true} 
  *rnd*
  (java.util.Random. 42))

(defn- uniform
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


;;;; Formulas

;; failed formulas report once per formula regardless how many generations were run
(after-silently
  (formula "some description" [a "y"]
    a => :foo))
(fact @reported => (one-of (contains {:type :mock-expected-result-failure
                                      :description "some description"})))

(defn make-string []
  (rand-nth ["a" "b" "c" "d" "e" "f" "g" "i"]))

(formula
  "can now use simple generative-style formulas"
  [a (make-string) b (make-string)]
  (str a b) => (has-prefix a))


;; passing formulas run the generator many times, and evaluate 
;; their body many times - number of generations is rebindable

(defn-verifiable y-maker [] "y")
(defn-verifiable my-str [s] (str s))

(binding [midje.ideas.formulas/*num-generations-per-formula* 77]
  (formula [a (y-maker)]
    (my-str a) => "y"))
(fact @y-maker-count => 77)
(fact @my-str-count => 77)

;; runs only as few times as needed to see a failure
(defn-verifiable z-maker [] "z")
(defn-verifiable my-identity [x] (identity x))

(after-silently 
  (formula [z (z-maker)]
    (my-identity z) => "clearly not 'z'"))
(fact @z-maker-count => 1)
(fact @my-identity-count => 1)