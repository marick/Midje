(ns user.fus-fact-metadata-and-quoting
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]
            [midje.parsing.1-to-explicit-form.metadata :refer :all]
            [midje.util.pile :as pile]
            [midje.data.compendium :as compendium]))


;;; Metadata and evaluation

;; Of user-supplied data, only the name is 'autoquoted'. The rest are evaled.
;; (Only a map, though, is a non-quoting form.)
;; Midje-generated metadata is appropriately quoted if not a non-quoting form.

(fact name "doc string" {:meta (+ 1 2) :symbol 'symbol} 1 => 1)
(fact :check-only-at-load-time
  (let [metadata (meta (compendium/last-fact-checked<>))]
    (:meta metadata) => 3
    (:symbol metadata) => 'symbol

    (:midje/name metadata) => "name"
    (:midje/description metadata) => "doc string"
    (:midje/source metadata) => '(fact name "doc string" {:meta (+ 1 2) :symbol 'symbol} 1 => 1)
    (:midje/file metadata) => string?
    (:midje/line metadata) => number?
    (:midje/guid metadata) => string?
    (:midje/namespace metadata) => (ns-name *ns*)))

;; Tabular facts turn into a fact wrapped around a number of generated facts.
;; The metadata must be preserved
(tabular name "doc string" {:meta (+ 1 2) :symbol 'symbol} (fact ?a => 1) ?a 1)
(fact :check-only-at-load-time
  (let [metadata (meta (compendium/last-fact-checked<>))]
    (:meta metadata) => 3
    (:symbol metadata) => 'symbol

    (:midje/name metadata) => "name"
    (:midje/description metadata) => "doc string"
    (:midje/source metadata) => '(tabular name "doc string" {:meta (+ 1 2) :symbol 'symbol} (fact ?a => 1) ?a 1)
    (:midje/file metadata) => string?
    (:midje/line metadata) => number?
    (:midje/guid metadata) => string?
    (:midje/namespace metadata) => (ns-name *ns*)))

;; The same is true of metadata that's attached to the enclosed fact.
;; The metadata must be preserved
(tabular (fact name "doc string" {:meta (+ 1 2) :symbol 'symbol} ?a => 1) ?a 1)
(fact :check-only-at-load-time
  (let [metadata (meta (compendium/last-fact-checked<>))]
    (:meta metadata) => 3
    (:symbol metadata) => 'symbol

    (:midje/name metadata) => "name"
    (:midje/description metadata) => "doc string"
    (:midje/source metadata) => '(tabular (fact name "doc string" {:meta (+ 1 2) :symbol 'symbol} ?a => 1) ?a 1)
    (:midje/file metadata) => string?
    (:midje/line metadata) => number?
    (:midje/guid metadata) => string?
    (:midje/namespace metadata) => (ns-name *ns*)))

;; "Background" forms within a prerequisite end up wrapping a newly-constructed
;; fact, so metadata must be preserved.

(unfinished f)
(fact name "doc string" {:meta (+ 1 2) :symbol 'symbol}
  (prerequisites (f 1 2) => 3)
  1 => 1)
(fact :check-only-at-load-time
  (let [metadata (meta (compendium/last-fact-checked<>))]
    (:meta metadata) => 3
    (:symbol metadata) => 'symbol

    (:midje/name metadata) => "name"
    (:midje/description metadata) => "doc string"
    (:midje/source metadata) => '(fact name "doc string" {:meta (+ 1 2) :symbol 'symbol} (prerequisites (f 1 2) => 3) 1 => 1)
    (:midje/file metadata) => string?
    (:midje/line metadata) => number?
    (:midje/guid metadata) => string?
    (:midje/namespace metadata) => (ns-name *ns*)))
