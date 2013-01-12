(ns midje.t-sweet
  (:use midje.sweet
        midje.util
        midje.test-util)
  (:require midje.internal-ideas.t-fakes
            [midje.clojure-test-facade :as ctf]
            [midje.repl :as repl]
            [midje.config :as config]
            [midje.util.ecosystem :as ecosystem]
            [midje.emission.api :as emit]
            [midje.emission.state :as state]
            [midje.ideas.metadata :as metadata]))

(fact "all of Midje's public, API-facing vars have docstrings"
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.sweet)))) => []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.semi-sweet)))) => []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.unprocessed)))) => []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.util)))) => []
  (map str (remove (comp :doc meta) (vals (ns-publics 'midje.repl)))) => [])

(silent-fact (+ 1 1) => 3)
(note-that fact-fails, (fact-expected 3), (fact-actual 2))

(facts
  (+ 10 10) => 20
  (+ 20 20) => 40)


(capturing-output
 (config/with-augmented-config {:visible-future true}
   (future-fact 1 => 2))
 (fact @test-output => #"WORK TO DO\S* at \(t_sweet"))
              
(capturing-output 
 (config/with-augmented-config {:visible-future true}
   (future-fact :some-metadata "fact name" 1 => 2))
 (fact @test-output => #"WORK TO DO\S* \"fact name\" at \(t_sweet"))

              


(silent-fact
   (+ 1 1) => 3
   (+ 11 "1") =future=> "12"  ;; does not fail
   (+ 111 1) => 112)
(note-that (fails 1 time), (fact-actual 2), (fact-expected 3))


(defn number [] )
(defn two-numbers [] 
  (+ (number) (number)))


(silent-fact
 (two-numbers) => nil
 (provided
   (number) =throws=> [1]))
(note-that fact-fails, (fact-captured-throwable-with-message #"Right side of =throws=> should extend Throwable"))
  
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

; http://github.com/marick/Midje/issues/#issue/2
(fact
  (expect (always-one 5) => 1
          (not-called a-fun)))

(binding [midje.semi-sweet/*include-midje-checks* false]
  (load "sweet_compile_out"))

  
;; It doesn't matter which namespace the => is in
(silent-fact (+ 1 1) midje.semi-sweet/=> 3)
(note-that fact-fails, (fact-actual 2), (fact-expected 3))

(silent-fact (+ 1 1) midje.sweet/=> 3)
(note-that fact-fails, (fact-actual 2), (fact-expected 3))

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
  (capturing-output
   (fact "about mathematics"
     (future-fact "do in future"
                  nil => 1))
   (fact @test-output => #"WORK TO DO.*about mathematics.*do in future")))

;; Background prerequisites
(unfinished check-f check-g check-h)
(defn ander [n]
  (and (check-f n) (check-g n) (check-h n)))

(against-background [(check-f 1) => true, (check-g 1) => true, (check-h 1) => true]
   (silent-fact
    (ander 1) => truthy
    (ander 1) => falsey (provided (check-f 1) => false)
    (ander 1) => falsey (provided (check-g 1) => false)
    (ander 1) => falsey (provided (check-h 1) => false)))


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

(defn second-arg [a b] (+ a (f a (g b))))
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
(note-that fact-fails, (prerequisite-called :times 1))
  
(silent-fact ":times can be a range"
 (called 1) => 1
 (provided
   (called 1) => 1 :times (range 2 8)))
(note-that fact-fails, (prerequisite-called :times 1))
 
(silent-fact "times can be a function"
 (do (called 1) (called 1) (called 1)) => 1
 (provided
   (called 1) => 1 :times even?))
(note-that fact-fails, (prerequisite-called :times 3))
  
(fact "by default, can be called zero or more times"
  (do (called 1) (called 1)) => 1
  (provided
    (called 1) => 1))

(silent-fact "how to say something is never called"
 (called 45) => 3
 (provided
   (called irrelevant) => 1 :times 0))
(note-that fact-fails, (prerequisite-called :times 1))

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


(without-externally-visible-changes
 (fact "inner fact functions also return true or false"
   (alter-var-root #'inners conj (fact 1 => 1 "ignored value"))
   (alter-var-root #'inners conj (fact 2 => 1 "ignored value"))))

(fact
  inners => [true false])

                         

                         ;;;; 
                         
(defn a [])
(defn b [] (a))

(fact "prerequisites can throw throwables"
  (b) => (throws Exception)
  (provided 
    (a) =throws=> (Exception.)))


;; Tool creators can hook into the maps generated by the Midje compilation process

(unfinished foo)
(defn-call-countable noop-fn [& args] :do-nothing)
(binding [midje.semi-sweet/*expect-checking-fn* noop-fn]
  (fact :ignored => :ignored
    (provided
      (foo) => :bar)))
(fact @noop-fn-count => 1)


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
  (#'midje.internal-ideas.t-fakes/var-inc-user 2) => 400
  (provided
    (#'midje.internal-ideas.t-fakes/var-inc 2) => 200))

(fact "and can fake local functions using vars"
  (#'var-inc-user-local 2) => 400
  (provided
    (#'var-inc-local 2) => 200))

(fact "default prerequisites work with vars"
  (config/with-augmented-config {:partial-prerequisites true}
    (#'midje.internal-ideas.t-fakes/var-twice) => 201
    (provided
      (#'midje.internal-ideas.t-fakes/var-inc 2) => 200)))

;;; Unfolded prerequisites

(fact "vars also work with unfolded prerequisites"
  (var-twice-local) => 201
  (provided
   (var-inc-local (var-inc-local 2))  => 201))

(defn here-and-there []
  (var-inc-local (#'midje.internal-ideas.t-fakes/var-inc 2)))
  
(fact "vars also work with unfolded prerequisites"
  (here-and-there) => 201
  (provided
   (var-inc-local (#'midje.internal-ideas.t-fakes/var-inc 2)) => 201))

(defn there-and-here []
  (#'midje.internal-ideas.t-fakes/var-inc (var-inc-local 2)))
  
(fact "vars also work with unfolded prerequisites"
  (there-and-here) => 201
  (provided
   (#'midje.internal-ideas.t-fakes/var-inc (var-inc-local 2)) => 201))

(defn over-there-over-there-spread-the-word-to-beware []
  (#'midje.internal-ideas.t-fakes/var-inc
   (#'midje.internal-ideas.t-fakes/var-inc 2)))
  
(fact "vars also work with unfolded prerequisites"
  (over-there-over-there-spread-the-word-to-beware) => 201
  (provided
   (#'midje.internal-ideas.t-fakes/var-inc
    (#'midje.internal-ideas.t-fakes/var-inc 2)) => 201))

 (fact "exceptions do not blow up"
   (odd? "foo") => (throws Exception)
   "foo" =not=> odd?)
   
;;; fact groups

(fact-group :integration {:timing 3}
            "strings do not set metadata in fact groups"
  metadata/metadata-for-fact-group => {:integration true
                                                   :timing 3})
            

                                        ;;; fact groups



(def integration-run-count (atom 0))
(def not-integration-run-count (atom 0))

(fact-group :integration
  (fact yes-integration
    (swap! integration-run-count inc))

  (fact no-integration {:integration false}
    (swap! not-integration-run-count inc)))



(ecosystem/when-1-3+
 (emit/silently
  ;; Don't step on the running count up to this point.
  (repl/check-facts *ns* :print-no-summary :integration))


 (fact
   :check-only-at-load-time
   @integration-run-count => 2
   @not-integration-run-count => 1)
)
