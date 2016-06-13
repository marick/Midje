(ns as-documentation.prerequisites.partial
  (:require [midje.config :as config]
            [midje.sweet :refer :all]
            [midje.test-util :refer :all]))

;; Sometimes the prerequisite function already exists. What should
;; happen if there's no prerequisite for a particular argument list?
;; Should it default to the existing function or not? The Midje users
;; who care prefer that such a case be an error:

(defn I-am-I-cried [n] n)

(defn using-function [n]
  (+ (I-am-I-cried n) (I-am-I-cried (inc n))))


(silent-fact
  (using-function 4) => (+ 80 4)
  (provided
    (I-am-I-cried 5) => 80))
(note-that fact-fails, some-prerequisite-was-called-with-unexpected-arguments)

;; However, it's also possible to ask that unmatched calls default to
;; the real values. The config/with-augmented-config simulates the
;; loading of a configuration file that sets `:partial-prerequisites` with
;; `change-defaults`.

(config/with-augmented-config {:partial-prerequisites true}
  (fact
    (using-function 4) => (+ 80 4)
    (provided
      (I-am-I-cried 5) => 80)))
