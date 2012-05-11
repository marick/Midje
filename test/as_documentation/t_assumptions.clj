(ns as-documentation.t-assumptions
  (:use midje.sweet
        midje.test-util))

(comment

  (fact "text"
    (examples-assume (alive? anything) => false)
    ...cell... => dies?
    (provided
      (...)))


  (prerequisite-group [(all-status) => ?desired-status]
    (status ...foo...) => ?desired-status
    (status ...bar...) => ?desired-status
    (status ...quux...) => ?desired-status)
  
  (fact "text"
    (examples-assume (all-status) => :dead)

    ...cell... => dies?
    )

  ;; Empty the log before every example

  (to-ensure [@log => empty?]
    (reset! log []))

(fact "something"
  (prerequisites (f 1) => 3
                 (f 2) => 4)
  (ensure-prerequisite (table :whatever) => full?)
  (g 3) => 5
  (provided
    (f 3) => 1))

(fact
  (allow-assumption-here (table :whim) => full))

(allow-assumption-here (clean database) => true)

)


