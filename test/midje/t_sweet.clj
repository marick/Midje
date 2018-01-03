(ns midje.t-sweet
  (:require [midje.sweet :refer :all]
            [midje.util :refer :all]
            [midje.test-util :refer :all]
            [midje.repl :as repl]
            [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]
            [midje.data.compendium :as compendium]
            ;; Following is for testing the use of vars to fake private functions
            ;; in another namespace.
            midje.data.t-prerequisite-state))

(fact "all of Midje's public, API-facing vars have docstrings"
  ;; At the moment, use of Potemkin to import vars breaks the following tests.
  ;; The vars are created even if not used.
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.sweet)))) =future=> []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.util)))) => []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.repl)))) =future=> [])





(defn number [] )
(defn two-numbers []
  (+ (number) (number)))


(silent-fact
  (two-numbers) => nil
  (provided
    (number) =throws=> [1]))
(note-that fact-fails (fact-captured-throwable-with-message
                        #"Right side of =throws=> should extend Throwable"))

(silent-fact "`=throws=>` provided arrows should fail when used outside of `provided` body"
  (inc 1) =throws=> 2)
(note-that fact-fails (fact-failed-with-note
                        #"The prerequisite arrow appears outside the body of a `provided`:\(inc 1\) =throws=> 2"))

(silent-fact "nested `=throws=>` provided arrows should fail when used outside of `provided` body"
  (let [x 1]
    (inc x) =throws=> 2))
(note-that fact-fails (fact-failed-with-note
                        #"The prerequisite arrow appears outside the body of a `provided`:\(inc x\) =throws=> 2"))

(silent-fact "`=streams=>` provided arrows should fail when used outside of `provided` body"
  (inc 1) =streams=> 2)
(note-that fact-fails (fact-failed-with-note
                        #"The prerequisite arrow appears outside the body of a `provided`:\(inc 1\) =streams=> 2"))

(facts "this is a doc string"
  (+ 10 10) => 20
  "this is another one"
  (+ 20 20) => 40)

(unfinished g)
(defn f [n] (g n))
(defn call2 [n m]
  (+ (g n) (g m)))

;; Some examples of prerequisites
(fact (f 1) => 33
  (provided (g 1) => 33))

(facts
  (f 1) => 313
  (provided
    (g 1) => 313)

  (f 22) => 500
  (provided
    (g 22) => 500))

(facts
  (call2 1 2) => 30
  (provided
    (g 1) => 10
    (g 2) => 20))

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
  (provided (g ...something...) => ...g-value...))

(fact "key-value pairs can be passed to override normal behavior"
  (always-one 3) => 3 :expected-result 1)

(defn a-fun [n] n)
; http://github.com/marick/Midje/issues/#issue/5
(doseq [v (range 5)]
  (fact
    (str "It should be the identity for value " v) ;; doc string needn't be constant
    (a-fun v) => v))

(fact (+ 1 2) =not=> 599)

(silent-fact (+ 1 2) =not=> 3)
(note-that fact-fails-because-of-negation, (fact-actual 3), (fact-expected 3))

(fact (+ 1 2) =not=> even?)

(silent-fact (+ 1 2) =not=> odd?)
(note-that fact-fails-because-of-negation, (fact-actual 3), (fact-expected 'odd?))

;; fact and future-fact descriptions nest themselves when reported

(silent-fact "A"
  (fact "B"
      (+ 1 2) => 1))
(note-that fact-fails, (fact-described-as "A" "B"))

(silent-fact "level 1"
  (fact "level 2"
    (fact "level 3"
      (throw (Exception. "BOOM")) => anything)))
(note-that fact-fails, (fact-described-as "level 1" "level 2" "level 3"))


(config/with-augmented-config {:visible-future true}
  (capturing-fact-output
   (fact "about mathematics"
     (future-fact "do in future"
                  nil => 1))
   (fact @fact-output => #"WORK TO DO.*about mathematics.*do in future")))

;; Background prerequisites

(unfinished h g i j)


(defn f [n] (+ (g (h n)) (i (h n))))

(fact
 (f 1) => 3
 (provided
   (g (h 1)) => 2
   (i (h 1)) => 1))

(defn nesty [n] (g (h (i (j n)))))
(fact
 (nesty 1) => 3
 (provided
   (g (h (i (j 1)))) => 3))

(defn f [n m] 100)
(defn second-arg [a b]
  (+ a (f a (g b))))

(fact
 (second-arg 1 2) => 9
 (provided
   (f 1 (g 2)) => 8))


(unfinished scope-to-fact)
(defn g [x] (scope-to-fact))

(facts
  (against-background (scope-to-fact) => 5)
  (g 1) => 5
  (let [result (g 1)] result => 5))    ;; This used to fail

(against-background [(scope-to-fact) => 5]
  (facts
    (g 1) => 5
    (let [result (g 1)] result => 5)))    ;; This used to fail

(background (scope-to-fact) => 5)
(facts
  (g 1) => 5
  (let [result (g 1)] result => 5)    ;; This used to fail
)

(background (scope-to-fact) => "outer")

(fact "fakes can be overridden"
  (against-background (scope-to-fact) => "middle")
  (g 1) => "middle"
  (g 1) => "inner"
  (provided
    (scope-to-fact) => "inner"))


;;; When prerequisites are called the wrong number of times.
(unfinished called)

(silent-fact
 (called 1) => 1
 (provided
   (called 1) => 1 :times 2))
(note-that fact-fails, (the-prerequisite-was-incorrectly-called 1 :time))

(silent-fact ":times can be a range"
 (called 1) => 1
 (provided
   (called 1) => 1 :times (range 2 8)))
(note-that fact-fails, (the-prerequisite-was-incorrectly-called 1 :time))

(silent-fact "times can be a function"
 (do (called 1) (called 1) (called 1)) => 1
 (provided
   (called 1) => 1 :times even?))
(note-that fact-fails, (the-prerequisite-was-incorrectly-called 3 :times))

(fact "by default, can be called zero or more times"
  (do (called 1) (called 1)) => 1
  (provided
    (called 1) => 1))

(silent-fact "how to say something is never called"
 (called 45) => 3
 (provided
   (called irrelevant) => 1 :times 0))
(note-that fact-fails, (the-prerequisite-was-incorrectly-called 1 :time))

                                ;;; Facts have return values

(config/with-augmented-config {:print-level :print-nothing}
 (without-changing-cumulative-totals

  (def should-be-true-because-of-fact-success
    (fact "fact returns true on success"
      (+ 1 1) => 2
      "some random return value"))

  (def should-be-false-because-of-fact-failure
    (fact "fact returns false on failure"
      (+ 1 1) => 3
      (+ 1 1) => 2
      "some random return value"))

  (def should-be-true-despite-previous-failue
    (fact "a fact's return value is not affected by previous failures"
      (+ 1 1) => 2
      "some random return value"))

  (def should-be-true-because-of-lower-levels
    (fact "an outer level fact is true if lower level facts are"
      (fact (+ 1 3) => 4)
      (fact (- 1 3) => -2)
      "some randome return value"))

  (def should-be-false-because-of-lower-level
    (fact "an outer level fact is false if any lower level facts are"
      (fact
        (fact (inc 1) => 1)
        "some random return value")
      (fact (inc 1) => 2)
      "some randome return value")
  )))

(fact
  should-be-true-because-of-fact-success => true
  should-be-false-because-of-fact-failure => false
  should-be-true-despite-previous-failue => true
  should-be-true-because-of-lower-levels => true
  should-be-false-because-of-lower-level => false)


(def inners [])

(fact "inner fact functions also return true or false"
  (alter-var-root #'inners conj (fact 1 => 1 "ignored value"))
  (alter-var-root #'inners conj (silent-fact 2 => 1 "ignored value")))
(fact inners => [true false])



                         ;;;;

(defn a [])
(defn b [] (a))

(fact "prerequisites can throw throwables"
  (b) => (throws Exception)
  (provided
    (a) =throws=> (Exception.)))


;; Tool creators can hook into the maps generated by the Midje compilation process

;; The :rhs of a prerequisite map is a little complicated because
;; it looks something like this `[55 :times 3 :position [file line]]. This parses
;; that into the true rhs and a override-map. (Maybe the tool writers would prefer
;; them separate?)
(defn augmented [prerequisite-map]
  (let [[true-rhs & overrides] (:rhs prerequisite-map)]
    (assoc prerequisite-map
           :true-rhs true-rhs
           :override-map (apply hash-map overrides))))

(with-local-vars [log {}]
  (config/with-augmented-config {:check-recorder
                                 (fn [example-map prerequisite-maps]
                                   (var-set log {:example-map example-map
                                                     :prerequisite-maps prerequisite-maps}))}
    (silent-fact (+ 1 2) =not=> (roughly 10)
      (provided
        (a 33) =streams=> [1 2] :times 3
        ..meta.. =contains=> {:a 33}))
    (let [example-map (:example-map (var-get log))
          prereqs (map augmented (:prerequisite-maps (var-get log)))]
      (fact
        example-map => (contains {:call-form '(+ 1 2)
                                  :arrow '=not=>
                                  :expected-result-form '(roughly 10)})
        prereqs => (just (contains {:call-form '(a 33)
                                    :arrow '=streams=>
                                    :true-rhs [1 2]
                                    :override-map (contains {:times 3})})
                         (contains {:call-form '..meta..
                                    :arrow '=contains=>
                                    :true-rhs {:a 33}}))))))

;; In prerequisites, functions can be referred to by vars as well as symbols

;;; These functions are duplicated in t_fakes.clj, since the whole
;;; point of allowing vars is to refer to private vars in another
;;; namespace. To make sure there's no mistakes, these local versions
;;; are so tagged.
;;;
(defn var-inc-local [x] (inc x))
(defn var-inc-user-local [x] (* x (var-inc-local x)))
(defn var-twice-local []
  (var-inc-local (var-inc-local 2)))

(fact "can fake private remote-namespace functions using vars"
  (#'midje.data.t-prerequisite-state/var-inc-user 2) => 400
  (provided
    (#'midje.data.t-prerequisite-state/var-inc 2) => 200))

(fact "and can fake local functions using vars"
  (#'var-inc-user-local 2) => 400
  (provided
    (#'var-inc-local 2) => 200))

(fact "default prerequisites work with vars"
  (config/with-augmented-config {:partial-prerequisites true}
    (#'midje.data.t-prerequisite-state/var-twice) => 201
    (provided
      (#'midje.data.t-prerequisite-state/var-inc 2) => 200)))

;;; Unfolded prerequisites

(fact "vars also work with unfolded prerequisites"
  (var-twice-local) => 201
  (provided
   (var-inc-local (var-inc-local 2))  => 201))

(defn here-and-there []
  (var-inc-local (#'midje.data.t-prerequisite-state/var-inc 2)))

(fact "vars also work with unfolded prerequisites"
  (here-and-there) => 201
  (provided
   (var-inc-local (#'midje.data.t-prerequisite-state/var-inc 2)) => 201))

(defn there-and-here []
  (#'midje.data.t-prerequisite-state/var-inc (var-inc-local 2)))

(fact "vars also work with unfolded prerequisites"
  (there-and-here) => 201
  (provided
   (#'midje.data.t-prerequisite-state/var-inc (var-inc-local 2)) => 201))

(defn over-there-over-there-spread-the-word-to-beware []
  (#'midje.data.t-prerequisite-state/var-inc
   (#'midje.data.t-prerequisite-state/var-inc 2)))

(fact "vars also work with unfolded prerequisites"
  (over-there-over-there-spread-the-word-to-beware) => 201
  (provided
   (#'midje.data.t-prerequisite-state/var-inc
    (#'midje.data.t-prerequisite-state/var-inc 2)) => 201))

 (fact "exceptions do not blow up"
   (odd? "foo") => (throws Exception)
   "foo" =not=> odd?)

;;; fact groups

(fact-group :integration {:timing 3}
            "strings do not set metadata in fact groups"
  (fact (inc 33) => 34)
  (let [stashed (compendium/last-fact-checked<>)]
    (fact (meta stashed) => (contains {:integration true :timing 3}))))


                                        ;;; fact groups



(def integration-run-count (atom 0))
(def not-integration-run-count (atom 0))

(fact-group :integration
  (fact yes-integration
    (swap! integration-run-count inc))

  (fact no-integration {:integration false}
    (swap! not-integration-run-count inc)))



(emit/silently
 ;; Don't step on the running count up to this point.
 (repl/check-facts *ns* :print-no-summary :integration))


(fact
  :check-only-at-load-time
  @integration-run-count => 2
  @not-integration-run-count => 1)
