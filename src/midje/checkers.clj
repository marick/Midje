(ns midje.checkers
  "Checkers are for checking results of checkables, or checking 
   that appropriate arguments are passed to prerequisites"
  (require [such.vars :as var]
           [such.immigration :as immigrate]))

(when-not (resolve '&)
  (let [docstring "This var is defined so that Midje prerequisites can use & for optional args without having to quote it."]
    (intern *ns* (vary-meta '& assoc :doc docstring) docstring)))

;; Immigrating specific vars to reduce the chance that a slipup in one of those
;; files results in polluting the checker namespace.

(immigrate/selection 'midje.checking.checkers.defining
                     '[defchecker checker as-checker])
(immigrate/selection 'midje.checking.checkers.chatty
                     '[chatty-checker])
(immigrate/selection 'midje.checking.checkers.simple
                     '[truthy falsey TRUTHY FALSEY anything irrelevant exactly throws roughly])
(immigrate/selection 'midje.checking.checkers.combining
                     '[every-checker some-checker])
(immigrate/selection 'midje.checking.checkers.collection
                     '[has has-suffix has-prefix just contains n-of
                       one-of two-of three-of four-of five-of six-of seven-of eight-of nine-of ten-of])
