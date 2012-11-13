(ns midje.ideas.t-rerunning-facts
  (:use [midje.sweet]
        [clojure.pprint]
        [midje.test-util]
        [midje.ideas.rerunning-facts]
        [midje.ideas.metadata :only [fact-name fact-true-name
                                     fact-source fact-namespace]]))


;;;;;   The interface

(forget-facts :all)

;;; Recently-run facts.

(def run-count (atom 0))
(fact
  (swap! run-count inc)
  (+ 1 1) => 2)
(recheck-fact)
(fact @run-count => 2)
(let [definition (source-of-last-fact-checked)]
  (fact definition => '(fact @run-count => 2)))

(def outer-run-count (atom 0))
(def inner-run-count (atom 0))
(fact "The last fact check is the outermost nested check"
  (swap! outer-run-count inc)
  (+ 1 1) => 2
  (fact "inner fact"
    (swap! inner-run-count inc)
    (fact (- 1 1) => 0)))
(recheck-fact)

(let [fact-name (:midje/name (meta last-fact-checked))]
  (fact
    "The last fact check is the outermost nested check"
    @outer-run-count => 2
    @inner-run-count => 2))


(def run-count (atom 0))
(fact "outermost"
  (fact "inner 1"
    (swap! run-count inc))
  (fact "inner 2"
    (swap! run-count inc)))
(recheck-fact)
(fact
  @run-count => 4)
  
(def run-count (atom 0))
(tabular "tabular facts count as last-fact checked"
  (fact
    (swap! run-count inc)
    (+ ?a ?b) => ?c)
  ?a ?b ?c
  1  2  3
  2  2  4)
(recheck-fact)
(fact @run-count => 4)

;; Facts mark themselves as last-fact-checked each time they're rechecked.

(fact (+ 1 1) => 2)
(def one-plus-one (last-fact-checked))
(fact (+ 2 2) => 4)
(def two-plus-two (last-fact-checked))

(recheck-fact)
(let [previous (last-fact-checked)]
  (fact previous => (exactly two-plus-two)))

(one-plus-one)
(let [previous (last-fact-checked)]
  (fact previous => (exactly one-plus-one)))

;; failures do not prevent later facts from being rechecked

(forget-facts)
(def succeed (atom false))
(def fail (atom false))

(run-silently
 (fact "1"
   (reset! fail :fail)
   (+ 1 2) => 3333))
(fact "2"
  (reset! succeed :succeed)
  (+ 1 2) => 3)

(reset! succeed false)
(reset! fail false)
(run-silently
 (check-facts))


(fact "Both facts were checked"
  @succeed => :succeed
  @fail => :fail)

;;; Namespace-oriented compendium

(forget-facts)

;; Nothing to do
(check-facts)
(check-facts *ns*)

(def named-fact-count (atom 0))
(def anonymous-fact-count (atom 0))


(defn redefine-facts []
  (forget-facts)
  (reset! named-fact-count 0)
  (reset! anonymous-fact-count 0)
  (fact "my fact"
    (swap! named-fact-count inc)
    (+ 1 1) => 2)

  (fact 
    (swap! anonymous-fact-count inc)
    (+ 1 1) => 2))

(redefine-facts)

(check-facts)
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)


(redefine-facts)
(check-facts *ns*)
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)

(redefine-facts)
(check-facts 'midje.ideas.t-rerunning-facts)
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)

(redefine-facts)
(check-facts 'clojure.core)
(fact @named-fact-count => 1)
(fact @anonymous-fact-count => 1)

(redefine-facts)
(check-facts *ns* *ns*)
(fact @named-fact-count => 3)
(fact @anonymous-fact-count => 3)

;;; Redefinitions

(forget-facts)

(def run-count (atom 0))

(fact "name"
  (swap! run-count inc))

(fact "name"
  (+ 1 1) => 2)

;; If there are two facts now defined, the run-count will increment when we do this:

(check-facts)

(fact
  @run-count => 1)

;; Redefinition to an identical form does not produce copies
(forget-facts)

(fact
  (swap! run-count inc)
  (+ 1 2) => 3)
(fact
  (swap! run-count inc)
  (+ 1 2) => 3)

(reset! run-count 0)
(check-facts)

(let [facts (fetch-matching-facts (constantly true))]
  (future-fact "There is still only one defined fact."
    (count facts) => 1
  @run-count => 1))



;;; Run facts matching a predicate

(def simple-fact-run-count (atom 0))
(def integration-run-count (atom 0))

(defn redefine-facts []
  (forget-facts)
  (reset! simple-fact-run-count 0)
  (reset! integration-run-count 0)
  (fact simple-fact
    (swap! simple-fact-run-count inc))
  (fact :integration
    (swap! integration-run-count inc)))

(redefine-facts)
(check-matching-facts #(-> % :midje/name (= "simple-fact")))
(fact
  @simple-fact-run-count => 2
  @integration-run-count => 1)


(redefine-facts)
(check-matching-facts :integration)
(fact
  @simple-fact-run-count => 1
  @integration-run-count => 2)

;;; forget-facts can operate on other than the default namespace.

(def sample-compendium '{user [some-fact], clojure.core [other-fact]})
(reset! by-namespace-compendium sample-compendium)

(forget-facts)

(let [stashed @by-namespace-compendium]
  (fact "forget-facts by default forgets the facts in this namespace"
    stashed => sample-compendium))

(forget-facts *ns*) ; forget the fact we just defined

(let [stashed @by-namespace-compendium]
  (fact "forget-facts can take a namespace"
    stashed => sample-compendium))

;; We can also forget namespaces by symbol
(reset! by-namespace-compendium sample-compendium)
(forget-facts 'user)
(let [stashed @by-namespace-compendium]
  (fact "forget-facts can take a namespace symbol"
    stashed => '{clojure.core [other-fact]}))

;; :all is a special case
(reset! by-namespace-compendium sample-compendium)
(forget-facts :all)
(let [stashed @by-namespace-compendium]
  (fact "forget-facts can take :all to forget everything"
    stashed => {}))

;;; fact groups

(forget-facts)

(fact-group :integration {:timing 3}
            "strings do not set metadata in fact groups"
  midje.ideas.metadata/metadata-for-fact-group => {:integration true
                                                   :timing 3})
            

(forget-facts)
(def integration-run-count (atom 0))
(def not-integration-run-count (atom 0))

(fact-group :integration
  (fact yes-integration
    (swap! integration-run-count inc))

  (fact no-integration {:integration false}
    (swap! not-integration-run-count inc)))

(check-matching-facts :integration)

(fact
  @integration-run-count => 2
  @not-integration-run-count => 1)


(facts "locating fact namespaces"
  (fact "defaults to test directory"
    (let [default-namespaces (fact-namespaces)]
      default-namespaces => (contains 'midje.ideas.t-rerunning-facts)
      default-namespaces =not=> (contains 'midje.ideas.rerunning-facts)))

  (fact "can be given explicit directories" 
    (let [chosen-namespaces (fact-namespaces "src")]
      chosen-namespaces =not=> (contains 'midje.ideas.t-rerunning-facts)
      chosen-namespaces => (contains 'midje.ideas.rerunning-facts))
    (let [chosen-namespaces (fact-namespaces "src" "test")]
      chosen-namespaces => (contains #{'midje.ideas.rerunning-facts
                                       'midje.sweet}
                                     :gaps-ok)
      chosen-namespaces => (contains 'behaviors.t-isolated-metaconstants)))

  (fact "can filter by prefix"
    (let [default-prefix (fact-namespaces :prefix "midje.checkers")]
      default-prefix => (contains 'midje.checkers.t-chatty)
      default-prefix =not=> (contains 'midje.ideas.t-rerunning-facts)
      default-prefix =not=> (contains 'midje.ideas.rerunning-facts))

    (let [chosen-prefix (fact-namespaces "src" :prefix "midje.ideas")]
      chosen-prefix => (contains 'midje.ideas.rerunning-facts)
      chosen-prefix =not=> (contains 'midje.checkers.chatty)
      chosen-prefix =not=> (contains 'midje.ideas.t-rerunning-facts))

    ;; truly a prefix
    (fact-namespaces "src" :prefix "ideas") => empty?))


