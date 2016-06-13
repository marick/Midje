(ns user.fus-check-after-creation
  (:require [midje.repl :refer :all]
            [midje.test-util :refer :all]
            [midje.config :as config]))

;;; The following forms obey :check-after-creation

(config/with-augmented-config {:check-after-creation false}
  (fact "fact" 1 => "fact")
  (facts "facts" 1 => "facts")
  (future-fact (throw (new Error ":check-after-creation fail")))
)

(silent-check-facts *ns* #"^fact$")
(note-that fact-failed (fact-expected "fact"))

(silent-check-facts *ns* #"^facts$")
(note-that fact-failed (fact-expected "facts"))

;; `with-state-changes` also obeys :check-after-creation
;; Note that the setup still applies to a fact run
;; outside the scope of the state changes.

(def state (atom 0))

(config/with-augmented-config {:check-after-creation false}
  (with-state-changes [(before :facts (swap! state inc))]
    (fact "state changes" 1 => "state changes")))

(fact @state => 0)

(silent-check-facts *ns* #"^state changes$")
(note-that fact-failed (fact-expected "state changes"))

;;; Check-after-creation makes no sense with contents.
(config/with-augmented-config {:check-after-creation false}
  (silent-with-state-changes [(before :contents (throw (new Error ":check-after-creation fail")))]
    (fact 1 => :never-executed)))

(note-that (fact-failed-with-note #"It is meaningless to combine.*with-state-changes.*:check-after-creation"))

