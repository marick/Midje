(ns midje.checkers.collection-util
  (:use [midje.util.form-utils :only [regex?]]
        [midje.checkers.extended-equality :only [extended-fn?]]
        [midje.checkers.chatty :only [as-chatty-falsehood]]))

(defn inexact-checker?
  "Can the checker potentially match non-unique elements
   in a seq? (Ex: regex #'a+' can match 'a' and 'aa'.)"
  [checker]
  (or (extended-fn? checker)
      (regex? checker)))

(defn total-match?
  "Have all the expected elements have been discovered?"
  [comparison]
  (= (count (:expected-found comparison))
     (count (:expected comparison))))

(defn closer-match?
  "Did the candidate match more expected elements than before?"
  [candidate best-so-far]
  (> (count (:actual-found candidate))
     (count (:actual-found best-so-far))))

(defn better-of [candidate best-so-far]
  (if (closer-match? candidate best-so-far) candidate best-so-far))

(defn collection-like?
  "Extend coll? to include strings."
  [thing]
  (or (coll? thing)
      (string? thing)))

(defn right-hand-singleton?
  "The kind of thing that, in (contains X), means (contains [X])"
  [thing]
  (or (not (coll? thing)) (map? thing)))

(defn same-lengths? [actual expected]
  (= (count actual) (count expected)))

(defn expected-fits?
  "Could expected fit as a subsequence of actual?"
  [actual expected]
  (>= (count actual) (count expected)))

(defn noted-falsehood
  "Produce a partially constructed chatty falsehood that contains
   a :notes key with the strings."
  [& strings ]
  (as-chatty-falsehood {:notes strings}))

(defn try-re
  "Use the function (re-find or re-matches) to apply re to the thing.
   If function blows up, return a chatty failure about it."
  [re x f]
  (try
    (f re x)
    (catch Exception ex
      (noted-falsehood (format "%s can't be used on %s, a %s."
                         (pr-str re) (pr-str x) (type x) ".")))))
