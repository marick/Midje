;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-formulas
  (:use midje.test-util
        midje.sweet
        midje.util.ecosystem))

;;;; Validation

(causes-validation-error #"There is no expection in your formula form"
  (formula [a 1]))

(causes-validation-error #"There is no expection in your formula form"
  (formula [a 1] 1))

(causes-validation-error #"There is no expection in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(causes-validation-error #"There is no expection in your formula form"
  (formula "vector fact" [a 1] (contains 3)))

(defn h [i])
(causes-validation-error #"There is no expection in your formula form"
  (formula "vector fact" [a 1] (contains 3)
    (provided (h anything) => 5)))

(causes-validation-error #"There are too many expections in your formula form"
  (formula "vector fact" [a 1] a => 1 a => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" :not-vector 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [a 1 1] 1 => 1))

(causes-validation-error #"Formula requires bindings to be an even numbered vector of 2 or more:"
  (formula "vector fact" [] 1 => 1))

(defn- gen-int [pred]
  (rand-nth (filter pred [-5 -4 -3 -2 -1 0 1 2 3 4 5])))

(formula
  "allows users to dynamically rebind to 1+"
  [n (gen-int #(< % 1))]
  (binding [midje.ideas.formulas/*num-generations-per-formula* n] nil) 
     => (throws #"must be an integer 1 or greater"))

(formula
  "binding too smalla value - gives nice error msg"
  [n (gen-int #(>= % 1))]
  (binding [midje.ideas.formulas/*num-generations-per-formula* n] nil) 
     =not=> (throws Exception))


;;;; Formulas

;; the first formula use ever!
(defn make-string []
  (rand-nth ["a" "b" "c" "d" "e" "f" "g" "i"]))
(formula "can now use simple generative-style formulas - with multipel bindings"
  [a (make-string) b (make-string) c (make-string)]
  (str a b c) => (has-prefix (str a b)))

(unfinished f)
(defn g [x] (str (f x) x))

(formula "'provided' works"
  [a (make-string)]
  (g a) => (str "foo" a)
  (provided 
    (f anything) => "foo"))


;; failed formulas report once per formula regardless how many generations were run
(after-silently
  (formula "some description" [a "y"] a => :foo))
(fact @reported => (one-of (contains {:type :mock-expected-result-failure
                                      :description "some description"})))


;; passing formulas run the generator many times, and evaluate 
;; their body many times - number of generations is rebindable
(defn-verifiable y-maker [] "y")
(defn-verifiable my-str [s] (str s))

(binding [midje.ideas.formulas/*num-generations-per-formula* 77]
  (formula [y (y-maker)]
    (my-str y) => "y"))
(fact @y-maker-count => 77)
(fact @my-str-count => 77)


;; runs only as few times as needed to see a failure
(defn-verifiable z-maker [] "z")
(defn-verifiable my-identity [x] (identity x))

(after-silently 
  (formula [z (z-maker)]
    (my-identity z) => "clearly not 'z'"))
(fact "calls generator once" @z-maker-count => 1)
(fact "evalautes body once" @my-identity-count => 1)


;; shrinks failure case to smallest possible failure
(when-1-3+
  (with-redefs [midje.ideas.formulas/shrink (constantly [0 1 2 3 4 5])]
    (after-silently
      (formula [x 100] 
        x => neg?)))
  (fact @reported => (one-of (contains {:type :mock-expected-result-functional-failure
                                        :actual 0}))))


;; shrunken failure case is in the same domain as the generator 
;; used to create the input case in the forst place.
(after-silently
  (formula [x (gen-int odd?)]  ;;(guard (gs/int) odd?)] 
    x => neg?))
(future-fact "shrunken failure case is in the same domain as the generator" 
  @reported => (one-of (contains {:type :mock-expected-result-failure
                                  :actual 1})))