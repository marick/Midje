(ns midje.checking.checkers.t-chatty
  (:require [midje.sweet :refer :all]
            [midje.util :refer :all]
            [midje.checking.core :refer :all]
            [midje.checking.checkers.defining :refer [checker?]]
            [midje.checking.checkers.chatty :refer [chatty-worth-reporting-on?
                                                    chatty-untease
                                                    chatty-checker?]]
            [midje.test-util :refer :all]))
(expose-testables midje.checking.checkers.chatty)

(facts "about chatty-checking utility functions"

  (chatty-untease 'g-101 '()) => [[] []]

  (chatty-untease 'g-101 '(1 (f) 33 (+ 1 2))) =>
                [ '( (f) (+ 1 2))  '(1 (g-101 0) 33 (g-101 1))  ])

(tabular
  (fact "knows if is worth reporting on"
    (chatty-worth-reporting-on? ?arg) => ?worth-it)

    ?arg   ?worth-it
    1      falsey
    '()    falsey
    '(f)   truthy
    ''(f)  falsey
    '[f]   falsey )


(tabular
  (fact "A single argument can be converted into a structured-form and a arg-value-name"
    (against-background (gensym 'symbol-for-destructured-arg) => 'unique-3)
    (let [[form name] (single-destructuring-arg->form+name ?original)]
      form => ?form
      name => ?name))
  ?original              ?form                       ?name
  'a                     'a                          'a
  '[a b]                 '[a b :as unique-3]         'unique-3
  '[a b & c :as all]     '[a b & c :as all]          'all
  '{:keys [a b]}         '{:keys [a b] :as unique-3} 'unique-3
  '{:keys [a b] :as all} '{:keys [a b] :as all}      'all
  ;; pathological cases
  '[a]                   '[a :as unique-3]           'unique-3
  '[a :as b]             '[a :as b]                  'b)




;; The form of chatty checkers

(def actual-plus-one-equals-4 (chatty-checker [actual] (= (inc actual) 4)))
(def no-longer-limited-form (chatty-checker [actual] (= (inc actual) 4 (+ 2 actual))))

(fact "chatty checkers are checkers"
  actual-plus-one-equals-4 => checker?)

(facts "about the form of chatty-checkers"
  actual-plus-one-equals-4 => chatty-checker?
  no-longer-limited-form => chatty-checker?)

(facts "about what chatty-checkers return"
  (actual-plus-one-equals-4 3) => true

  (let [result (actual-plus-one-equals-4 4)]
    result => data-laden-falsehood?
    result => {:actual 4
              :intermediate-results [ ['(inc actual) 5] ] })

  (let [result (no-longer-limited-form 4)]
    result => data-laden-falsehood?
    result => {:actual 4
              :intermediate-results [ ['(inc actual) 5] ['(+ 2 actual) 6] ]}))

;; Destructuring arguments

(def actual-plus-one-equals-4 (chatty-checker [actual] (= (inc actual) 4)))

(def vec-structured-checker
     (chatty-checker [ [a b & c]]
        (and (= a 1)
             (= b 2)
             (= c [3 4]))))

(def map-structured-checker
     (chatty-checker [{:keys [a b]}]
        (and (= a 1)
             (= b 2))))

(def map-structured-checker-with-as
     (chatty-checker [{:keys [a b] :as both}]
        (and (= a 1)
             (= b 2))))

(def other-map-structured-checker
     (chatty-checker [{a :a b :b}]
        (and (= a 1)
             (= b 2))))

(fact "chatty checkers can use a destructuring argument"
  ;; Note: Can't use extended-= because it swallows chatty-failures
  (= (vec-structured-checker [1 2 3 4]) true) => truthy )

(tabular "chatty checkers can use a map destructuring argument"
  (fact
      (= (?structured-checker {:a 1 :b 2}) true) => truthy
      (= (?structured-checker {:a 10 :b 10}) true) => falsey)

  ?structured-checker
  map-structured-checker
  map-structured-checker-with-as
  other-map-structured-checker )

(tabular
  (fact "different parts are in fact checked"
    (let [result (vec-structured-checker ?actual)]
      (= result true) => falsey
      (:actual result) => ?actual))
  ?actual
  ['x 2 3 4]
  [1 'x 3 4]
  [1 2 3 4 'x])

(fact "folded results are still shown"
  (:intermediate-results (vec-structured-checker ['x 2 3 4]))
  => '(    ((= a 1) false)
           ((= b 2) true)
           ((= c [3 4]) true) ) )

(tabular "map structured checkers still work"
  (fact (:intermediate-results (?structured-checker {:a 10 :b 2}))
  => '(    ((= a 1) false)
           ((= b 2) true) ))

  ?structured-checker
  map-structured-checker
  map-structured-checker-with-as
  other-map-structured-checker )


;; Chatty checkers: interaction with checkers that return chatty-failures.
(defn rows-in [rows] rows)
(defn has-rows [rows]
   (chatty-checker [actual-datastate]
                   ( (just (contains rows)) (rows-in actual-datastate))))

(silent-fact
  (let [all (fn [name] [{:region 1, :name name}])]
    (all "PRK") => (has-rows [ {:region 1, :name "GDR"} ])))
(note-that fact-fails, (fact-actual [{:region 1, :name "PRK"}]),
           (fact-gave-intermediate-result (rows-in actual-datastate) => [{:region 1, :name "PRK"}]))



;; Old bug. During midjcoexpansion/macroexpansion, the interior of a chatty checker
;; can turn into a lazy seq, which should be treated as if it were a list. This checks that.
(silent-fact
 (let [a-chatty-checker (fn [expected]
                          (chatty-checker [actual] (= expected (+ 1 actual))))]
   3 => (a-chatty-checker 33)))
(note-that fact-fails, (fact-actual 3),
           (fact-gave-intermediate-result (+ 1 actual) => 4))


;;; Parse errors


(fact "not a function"
  (macroexpand '(chatty-checker [n] (let [a 1] (= (inc a) (inc 1)))))
  => (throws #"Chatty checkers can't be used.*special forms.")
  (macroexpand '(chatty-checker [n] (cond ...)))
  => (throws #"Chatty checkers can't be used.*special forms.")

  (fact "`and` and `or`  are still OK, though."
    (macroexpand '(chatty-checker [n] (and 1 2))) =not=> (throws)
    (macroexpand '(chatty-checker [n] (or 1 2))) =not=> (throws)))




