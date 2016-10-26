(ns user.fus-shape-checking
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.shape-checkers :as c]
            [midje.util.ecosystem :refer [when-1-7+]]))

(comment

(when-1-7+

(silent-fact
  (let [expected {[:a :b] [even? neg?]
                   :c      c/required-path}

        actual (vector {:a {:b 1}   :c 3}
                       {:a {:b -2}}
                       {:a {:b 2}   :c 3})]

    actual => (c/all-built-like expected)))
(note-that fact-fails)
(note-that (fact-failed-with-note #"\[0 :a :b\]"))
(note-that (fact-failed-with-note #"\[1 :c\]"))
(note-that (fact-failed-with-note #"\[2 :a :b\]"))

)
)
