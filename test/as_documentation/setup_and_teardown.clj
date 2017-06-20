(ns as-documentation.setup-and-teardown
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

(def state (atom nil))


                                        ;;; setup

(with-state-changes [(before :facts (reset! state 0))]
  (fact "the state gets set before this fact"
    @state => 0)

  (fact "it also gets set before this fact"
    @state => 0))

;;; It's common to put `with-state-changes` inside an outermost fact

(facts swap!
  (with-state-changes [(before :facts (reset! state 0))]
    (fact "uses a function to update the current value"
      (swap! state inc)
      @state => 1)

    (fact "that function can take additional args"
      (swap! state - 33)
      @state => -33)

    (fact "swap returns the new value"
      (swap! state inc) => 1)))

(def log (atom :undefined))

(fact "*all* nested setups are run before *each* fact"
  (with-state-changes [(before :facts (reset! log []))]
    (fact "an outer fact"
      (swap! log conj "this will get overwritten")

      (with-state-changes [(before :facts (swap! log conj "from inner with-state-changes"))]
        (fact
          ;; the outer `before` has just reset the log
          @log => ["from inner with-state-changes"])))))


                                        ;;; teardown

;; Combining setup and teardown.

(fact "whereas `before` executes outer-to-inner, `after` is the reverse"
  (with-state-changes [(before :facts (reset! log ["outer in"]))
                       (after :facts (swap! log conj "outer out"))]
    (with-state-changes [(before :facts (swap! log conj "  inner in"))
                         (after :facts (swap! log conj "  inner out"))]
      (fact (+ 1 1) => 2)))
  @log => ["outer in"
           "  inner in"
           "  inner out"
           "outer out"])

(fact "teardown is executed even if the enclosed fact throws an exception."
  (try
    (with-state-changes [(after :facts (reset! log ["caught!"]))]
      (fact
        (throw (new Error))))
  (catch Error ex))
  @log => ["caught!"])


(fact "teardown is NOT executed when the corresponding `before` throws an exception."
  ;; Use `around` instead.
  (try
    (with-state-changes [(before :facts (do (reset! log [])
                                            (throw (new Error))))
                         (after :facts (reset! log ["caught!"]))]
      (fact))
  (catch Error ex))
  @log =not=> ["caught!"])

                                        ;;; namespace-state-changes

;;; The earlier example of testing `swap!` surrounded three facts with
;;; an `with-state-changes`. That prevents you from using only one of
;;; the facts in the repl.  An alternative to awkward editing is to
;;; use `namespace-state-changes`. Rather than being lexically scoped,
;;; it affects all fact definitions from the moment it's seen until the
;;; momment it's cancelled.

(namespace-state-changes [(before :facts (reset! state 0))])

;;; Note that the syntax is the same as for `with-state-changes`. Now you
;;; can use a fact that assumes a starting mutable state:

(fact "uses a function to update the current value"
  (swap! state inc)
  @state => 1)

;;; Note that any use of `namespace-state-changes` erases the
;;; previous one. For example, the following doesn't add teardown to
;;; the earlier setup. There is no longer any setup, just teardown.

(namespace-state-changes [(after :facts (swap! state inc))])

(reset! state 1000)
(fact "the `before` no longer happens"
  @state => 1000)

(fact "... but the `after` did"
  @state => 1001)

;;; Therefore, to "erase" the background, you can do this:

(namespace-state-changes [])
(fact @state => 1002)
(fact @state => 1002)


;;; This form is also acceptable:

(namespace-state-changes)

;;; `namespace-state-changes doesn't actually require the square brackets.

(defn halts?
  "Form is a function definition: '(fn [...] ...)
  `halts?` determins if that function will halt when applied to the given args."
  [form & args]
  true)

(fact "`halts` determines if a program halts"
  (halts? '(fn [n] (+ n 2)) 1) => true
  (halts? '(fn recursive [n] (recursive (dec n))) 1) =future=> false)
