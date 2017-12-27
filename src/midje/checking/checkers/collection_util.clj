(ns midje.checking.checkers.collection-util
  (:require [such.types :as types]
            [midje.checking.core :refer :all]))

(defn same-lengths? [actual expected]
  (= (count actual) (count expected)))

(defn inexact-checker?
  "Can the checker potentially match non-unique elements
   in a seq? (Ex: regex #'a+' can match 'a' and 'aa'.)"
  [checker]
  (or (types/extended-fn? checker)
      (types/regex? checker)))

(defn total-match?
  "Have all the expected elements been discovered?"
  [comparison]
  (same-lengths? (:expected-found comparison) (:expected comparison)))

(letfn [(closer-match?
         ;; Did the candidate match more expected elements than before?
         [candidate best-so-far]
         (> (count (:actual-found candidate))
            (count (:actual-found best-so-far)))) ]

  (defn better-of [candidate best-so-far]
    (if (closer-match? candidate best-so-far) candidate best-so-far)))

(defn collection-like?
  "Extend coll? to include strings."
  [x]
  (or (coll? x) (string? x)))

(defn right-hand-singleton?
  "The kind of thing that, in (contains X), means (contains [X])"
  [x]
  (or (not (coll? x)) (map? x)))

(defn expected-fits?
  "Could expected fit as a subsequence of actual?"
  [actual expected]
  (>= (count actual) (count expected)))

(defn noted-falsehood
  "Produce a partially constructed chatty falsehood that contains
   a :notes key with the strings."
  [& strings ]
  (as-data-laden-falsehood {:notes strings}))

(defn try-re
  "Use the function (re-find or re-matches) to apply re to the thing.
   If function blows up, return a chatty failure about it."
  [re x f]
  (try
    (f re x)
    (catch Exception ex
      (noted-falsehood (format "%s can't be used on %s, a %s."
                         (pr-str re) (pr-str x) (type x) ".")))))
