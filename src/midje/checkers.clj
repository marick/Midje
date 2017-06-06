(ns midje.checkers
  "Checkers are for checking results of checkables, or checking
   that appropriate arguments are passed to prerequisites"
  (:require [such.vars :as var]
            [such.immigration :as immigrate])
  (:require midje.checking.checkers.defining
            midje.checking.checkers.chatty
            midje.checking.checkers.simple
            midje.checking.checkers.combining
            midje.checking.checkers.collection))

(when-not (resolve '&)
  (let [docstring "This var is defined so that Midje prerequisites can use & for optional args without having to quote it."]
    (intern *ns* (vary-meta '& assoc :doc docstring) docstring)))

;; Immigrating specific vars to reduce the chance that a slipup in one of those
;; files results in polluting the checker namespace.

(immigrate/import-vars [midje.checking.checkers.defining
                          defchecker checker as-checker]
                       [midje.checking.checkers.chatty
                          chatty-checker]
                       [midje.checking.checkers.simple
                          truthy falsey TRUTHY FALSEY anything irrelevant exactly throws roughly]
                       [midje.checking.checkers.combining
                          every-checker some-checker]
                       [midje.checking.checkers.collection
                          has has-suffix has-prefix just contains n-of
                          one-of two-of three-of four-of five-of six-of
                          seven-of eight-of nine-of ten-of])
