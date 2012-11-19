(ns midje.t-repl
  (:use [midje.sweet]
        [midje.repl]
        [clojure.pprint]
        [midje.test-util]
        [midje.ideas.metadata :only [fact-name fact-source fact-namespace]]
        [midje.ideas.rerunning-facts :only [last-fact-function-run
                                            record-fact-existence
                                            namespace-facts
                                            compendium-contents]]))

(forget-facts :all)

                                ;;; Rechecking last-checked fact

(def run-count (atom 0))

(fact
  (swap! run-count inc)
  (+ 1 1) => 2)

(recheck-fact)

(fact @run-count => 2)

(let [definition (source-of-last-fact-checked)]
  (fact definition => '(fact @run-count => 2)))


;; Nesting of facts and most-recently-run fact

(def outer-run-count (atom 0))
(def inner-run-count (atom 0))
(fact "The last fact checked is the outermost nested check"
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

;; Multiple nested facts

(def run-count (atom 0))
(fact "outermost"
  (fact "inner 1"
    (swap! run-count inc))
  (fact "inner 2"
    (swap! run-count inc)))
(recheck-fact)
(fact
  @run-count => 4)


;; Tabular facts count as nested facts

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

;; Facts mark themselves as last-fact-checked each time they're
;; rechecked.  Note that a new fact-function is spawned off for
;; storage each time a fact is run. This is a side effect of the need
;; to keep metadata around. There may be a way to avoid the
;; duplication, but I don't see it right now.
(fact (+ 1 1) => 2)
(def one-plus-one (last-fact-checked))
(fact (+ 2 2) => 4)
(def two-plus-two (last-fact-checked))

(recheck-fact)
(let [previous (last-fact-checked)]
  (fact (fact-source previous) => (fact-source two-plus-two)))

(one-plus-one)
(let [previous (last-fact-checked)]
  (fact (fact-source previous) => (exactly (fact-source one-plus-one))))


                                ;;; Which facts are stored in the compendium

;; Nested facts are not.

(forget-facts)

(def inner-count (atom 0))
(def outer-count (atom 0))

(fact "outer"
  1 => 1
  (swap! outer-count inc)
  (fact "inner"
    (swap! inner-count inc)
    2 => 2))

(let [contents (compendium-contents)]
  (fact "only outer fact is available"
    (count contents) => 1
    (:midje/name (meta (first contents))) => "outer"))

;; Both get run, though.
(unobtrusive-check-facts)

(fact
  @outer-count => 2
  @inner-count => 2)



                                ;;; Running facts from the compendium
(forget-facts)

;; Nothing to do
(unobtrusive-check-facts)
(unobtrusive-check-facts *ns*)

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
 (unobtrusive-check-facts))


(fact "Both facts were checked"
  @succeed => :succeed
  @fail => :fail)




;;; Variant ways of using unobtrusive-check-facts with namespaces

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
    (+ 2 2) => 4))

(redefine-facts)   

(unobtrusive-check-facts)                           ; No namespace runs everything
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)


(redefine-facts)
(unobtrusive-check-facts *ns*)                      ; Explicit namespace arg
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)

(redefine-facts)
(unobtrusive-check-facts (ns-name *ns*))             ; Symbol namespace name
(fact @named-fact-count => 2)
(fact @anonymous-fact-count => 2)

(redefine-facts)
(unobtrusive-check-facts 'clojure.core)             ; A different namespace
(fact @named-fact-count => 1)           ; (no rerunning - only initial redefinition)
(fact @anonymous-fact-count => 1)

(redefine-facts)
(unobtrusive-check-facts *ns* *ns*)                 ; Multiple args
(fact @named-fact-count => 3)           ; (repeating same runs it again)
(fact @anonymous-fact-count => 3)


                                ;;; How one redefines facts
(forget-facts)

(def run-count (atom 0))

(fact "name"
  (swap! run-count inc))

(fact "name"
  (+ 1 1) => 2)

;; If two facts were now defined, the run-count would
;; increment when we do this:

(unobtrusive-check-facts)
(fact "But only one is defined"
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
(unobtrusive-check-facts)

(let [facts (fetch-matching-facts (constantly true))]
  (future "There is still only one defined fact."
    (count facts) => 1
  @run-count => 1))


                                ;;; Run facts matching a predicate


(def unobtrusive-fact-run-count (atom 0))
(def integration-run-count (atom 0))

(defn redefine-facts []
  (forget-facts)
  (reset! unobtrusive-fact-run-count 0)
  (reset! integration-run-count 0)
  (fact unobtrusive-fact
    (swap! unobtrusive-fact-run-count inc))
  (fact :integration
    (swap! integration-run-count inc)))



(redefine-facts)
(check-matching-facts #(-> % :midje/name (= "unobtrusive-fact")))
(fact
  @unobtrusive-fact-run-count => 2
  @integration-run-count => 1)

(redefine-facts)
(check-matching-facts :integration)
(fact
  @unobtrusive-fact-run-count => 1
  @integration-run-count => 2)


                                   ;;; forget-facts

(forget-facts)

(fact "in-this-namespace"
  1 => 1)
(def other-namespace-fact (vary-meta (last-fact-function-run)
                                     assoc
                                     :midje/name "other namespace fact"
                                     :midje/namespace 'clojure.core))
(record-fact-existence other-namespace-fact)

(forget-facts *ns*)
(let [this-result (namespace-facts *ns*)
      other-result (namespace-facts 'clojure.core)]
  (fact this-result => empty?)
  (fact other-result => [other-namespace-fact]))

;; We can also forget namespaces by symbol
(forget-facts 'clojure.core)
(fact (namespace-facts 'clojure.core) => [])


;; :all is a special case
(forget-facts :all)
(fact 1 => 1)
(record-fact-existence other-namespace-fact)

(fact
  (count (compendium-contents)) => 3) ; including self.

(forget-facts :all)
(let [result (compendium-contents)]
  (fact result => empty?))

