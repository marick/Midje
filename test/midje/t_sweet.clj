;; -*- indent-tabs-mode: nil -*-

(ns midje.t-sweet
  (:use [midje.sweet])
  (:use [midje.test-util]))

(after-silently ; failing
 (fact (+ 1 1) => 3)
 (fact @reported => (one-of bad-result)))

(after-silently ; succeeding
 (facts
   (+ 10 10) => 20
   (+ 20 20) => 40)
 (fact @reported => (two-of pass)))

(facts "this is a doc string"
  (+ 10 10) => 20
  "this is another one"
  (+ 20 20) => 40)

;; Results of facts are truth values.
(let [truth-values (list (run-silently (fact (+ 1 1) => 2))
                         (run-silently (fact (+ 1 2) => 2))
                         (run-silently (fact
                                         (+ 1 2) => 2
                                         (+ 1 2) => 3)))]
  (fact truth-values => [true false false]))

(unfinished g)
(defn f [n] (g n))
(defn call2 [n m]
  (+ (g n) (g m)))
  
;; Some examples of prerequisites
(after-silently
 (fact (f 1) => 33
   (provided (g 1) => 33))
 (fact @reported => (one-of pass)))

(after-silently
 (facts 
   (f 1) => 313
   (provided
     (g 1) => 313)
    
   (f 22) => 500
   (provided 
     (g 22) => 500))
 (fact @reported => (two-of pass)))

(after-silently 
 (facts 
   (call2 1 2) => 30
   (provided 
     (g 1) => 10
     (g 2) => 20))
 (fact @reported => (one-of pass)))

;; facts can see their environment
(let [outer-value 2]
  (fact
    (let [inner-value 3]
      (call2 outer-value inner-value) => 23
      (provided (g outer-value) => (* 10 outer-value)
                (g inner-value) => inner-value))))


(defn always-one [x] 1)
(defn g-caller [x] (g x))

; Metaconstants
(fact (always-one ...anything...) => 1)
(fact (g-caller ...something...) => ...g-value...
  (provided (g-caller ...something...) => ...g-value...))

(fact "key-value pairs can be passed to override normal behavior"
  (always-one 3) => 3 :expected-result 1)

(defn some-fn [n] n)
; http://github.com/marick/Midje/issues/#issue/5
(doseq [v (range 5)]
  (fact
    (str "It should be the identity for value " v) ;; doc string needn't be constant
    (some-fn v) => v))

; http://github.com/marick/Midje/issues/#issue/2
(fact
  (expect (always-one 5) => 1
          (not-called some-fn)))

(binding [midje.semi-sweet/*include-midje-checks* false]
  (load "sweet_compile_out"))

  
;; It doesn't matter which namespace the => is in
(after-silently 
 (fact (+ 1 1) midje.semi-sweet/=> 3)
 (fact @reported => (one-of bad-result)))

(after-silently 
 (fact (+ 1 1) midje.sweet/=> 3)
 (fact @reported => (one-of bad-result)))

(after-silently
 (fact (+ 1 2) =not=> 599)
 (fact @reported => (one-of pass)))

(after-silently
 (fact (+ 1 2) =not=> 3)
 (fact @reported => (one-of inappropriate-equality)))

(after-silently
 (fact (+ 1 2) =not=> even?)
 (fact @reported => (one-of pass)))

(after-silently
 (fact (+ 1 2) =not=> odd?)
 (fact @reported => (one-of inappropriate-checker)))

;; Background prerequisites
(unfinished check-f check-g check-h)
(defn ander [n]
  (and (check-f n) (check-g n) (check-h n)))

(after-silently
 (against-background [(check-f 1) => true, (check-g 1) => true, (check-h 1) => true]
   (facts
     (ander 1) => truthy
     (ander 1) => falsey (provided (check-f 1) => false)
     (ander 1) => falsey (provided (check-g 1) => false)
     (ander 1) => falsey (provided (check-h 1) => false)))
 (fact (four-of pass)))


