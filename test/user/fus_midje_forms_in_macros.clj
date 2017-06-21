(ns user.fus-midje-forms-in-macros
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.parsing.1-to-explicit-form.parse-background :as parse-background]))

;; Because of the way that Midje does its parsing, there are
;; complications when a user writes macros that wrap Midje forms. This
;; shows that some of those complications are handled.

;; This is pretty incomplete so far.

                                ;;; against-background

;; A macro with a let in it that surrounded a fact used to blow up.

(def random-atom (atom 0))
(def count-of-facts-checked (atom 0))

(defmacro with-memory-store [& body]
  `(let [increment# 1]
     (with-state-changes [(before :facts (reset! random-atom increment#))
                          (after :facts (swap! count-of-facts-checked inc))]
       ~@body)))

(with-memory-store
  (fact "one fact"
    @random-atom => 1)
  (fact "another"
    @count-of-facts-checked => 1))

(fact @count-of-facts-checked => 2)




;; Putting a fact inside a macro should not lead to an erroneous parse error

(defmacro hidden-fact [& body]
  `(fact ~@body))

(silent-with-state-changes []
  (hidden-fact 1 => 1))
(note-that parse-error-found)

;; That can be prevented with `add-midje-fact-symbols`:

(add-midje-fact-symbols '[hidden-fact])
(with-state-changes []
  (hidden-fact 1 => 1))
(parse-background/remove-midje-fact-symbols '[hidden-fact])

;;; Here is an old bug

(def db (atom 0))

(defmacro with-memory-store [& body]
  `(let [db# @db]
    (facts
      (with-state-changes [(before :contents (reset! db 3))
                           (after :facts (fn []))
                           (after :contents (reset! db db#))]
        ~@body))))

(with-memory-store
  (fact 1 => 1))

(defmacro with-server [& body]
  `(let [server# (fn [])]
     (facts
       (with-state-changes [(after :contents (server#))]
         ~@body))))

(with-server
  (fact 1 => 1))

(with-server (with-memory-store (fact 1 => 1)))
