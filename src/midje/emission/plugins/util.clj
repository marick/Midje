(ns ^{:doc "General purpose plugin utilities"}
  midje.emission.plugins.util
  (:use midje.clojure.core
        [clojure.repl :only [demunge]])
  (:require [clojure.string :as str]
            [midje.util.pile :as pile]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.colorize :as color]
            [midje.util.exceptions :as exception]
            gui-diff.internal))


;; The theory here was that using clojure.test output would allow text
;; from failing *facts* to appear within the clojure.test segment of
;; summary output. That doesn't work (though it works fine for
;; clojure.test output). The whole rigamarole is boring and I don't
;; care to jump through any more hoops. I say it's spinach, and I say
;; to hell with it.

;; (Don't change this to println, though. That doesn't work either.
;; Newlines are lost.)
(def emit-one-line ctf/output)
(defn emit-lines [lines]
  (doseq [line lines] (emit-one-line line)))

(defn indented [lines]
  (map (partial str "        ") lines))

(def sorted-if-appropriate gui-diff.internal/nested-sort)

(defn linearize-lines
  "Takes a nested structure that contains nils and strings.
   Converts it into a simple sequence of strings."
  [messy-lines]
  (->> messy-lines flatten (remove nil?)))

(defn function-name
  "Convert a function into a readable name, if possible."
  [function]
  (let [printed (pr-str function)
        [_ match] (re-matches #"#<([^/]+/[^ ]+).*" (demunge printed))]
  match))

(defn prerequisite-var-description
  "Takes a var naming a prerequisite and returns a string useful for printing"
  [prerequisite-var]
  ;; This emphasizes a little that the prerequisite is a var, without having
  ;; too much spewage. While it's nice to be able to write:
  ;;    (provided (foo 3) => 4)
  ;; ... that can cause confusion in those (relatively uncommon) cases where
  ;; rebinding `#'foo` does not change the value of `foo`. Having been reminded
  ;; occasionally that she's working with vars might help a perplexed user
  ;; become unperplexed.
  (let [name (pile/object-name prerequisite-var)]
    (if name
      (str "#'" name)
      (pr-str prerequisite-var))))

(defn attractively-stringified-value
  "Does some standard prettification of forms:
        : a function named `foo`
        : a nicely printed stack trace
        : maps and sets sorted by key."
  [value]
  (pred-cond value
    fn?                           (function-name value)
    exception/captured-throwable? (exception/friendly-stacktrace value)
    :else                         (pr-str (sorted-if-appropriate value))))

(defn format-nested-descriptions
  "Takes vector like [\"about cars\" nil \"sports cars are fast\"] and returns non-nils joined with -'s
   => \"about cars - sports cars are fast\""
  [nested-description-vector]
  (when-let [non-nil (seq (remove nil? nested-description-vector))]
    (str/join " - " non-nil)))

(defn filename-lineno
  "The ordinary way to describe the location of a failure."
  [[filename line-num]]
  (format "(%s:%s)" filename line-num))

(defn position-str
  "Describe a failure with its namespace"
  [[filename line-num] namespace]
  (let [namespace (if namespace (str "  " namespace) "")]
      (format "(%s:%s%s)" filename line-num namespace)))

;; TODO: The binding-note comes pre-formatted. Would probably be better
;; if the formatting were done here.
(defn failure-notice
  "The reader's eye is guided by a bright red FAIL, the filename and lineno, and
   perhaps this other information:
     : the descriptions of all enclosing facts, if any
     : notes on which bindings were supplied to a tabular fact"
  [m]
  (let [description (when-let [doc (format-nested-descriptions (:description m))]
                      (str (pr-str doc) " "))
        position (position-str (:position m) (:namespace m))
        table-substitutions (when-let [substitutions (:binding-note m)]
                              (str "With table substitutions: " substitutions))]
    (list
     (str "\n" (color/fail "FAIL") " " description "at " position)
     table-substitutions)))
