;; -*- indent-tabs-mode: nil -*-

(ns midje.checkers)

(defn republish* [namespace symbols]
  (require namespace)
  (doseq [sym symbols]
    (let [var ( (ns-publics namespace) sym)]
      (let [sym (with-meta sym (assoc (meta var) :ns *ns*))]
        (if (.hasRoot var)
          (intern *ns* sym (.getRoot var))
          (intern *ns* sym))))))

(defmacro republish [namespace & symbols]
  `(republish* '~namespace '~symbols))

(republish midje.checkers.chatty
           chatty-checker)
(republish midje.checkers.simple
           truthy falsey anything exactly throws roughly)
(republish midje.checkers.collection
           has has-suffix has-prefix just contains
           one-of two-of three-of four-of five-of six-of seven-of eight-of nine-of ten-of
           n-of)
(republish midje.checkers.deprecated
           map-containing only-maps-containing maps-containing in-any-order)
