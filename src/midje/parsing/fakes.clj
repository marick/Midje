(ns ^{:doc "An intermediate stage in the compilation of prerequisites."}
  midje.parsing.fakes
  (:use [utilize.seq :only (separate find-first)]
        [midje.util.object-utils :only [object-name]]
        [midje.checkers :only [exactly]]
        [midje.checkers.defining :only [checker? checker-makers]]
        [midje.parsing.expects :only [expect? up-to-full-expect-form]]
        [midje.util.form-utils :only [first-named? translate-zipper map-difference
                                      hash-map-duplicates-ok pred-cond
                                      quoted-list-form? extended-fn?]]
        [midje.checkers.extended-equality :only [extended-= extended-list-=]]
        [midje.internal-ideas.file-position :only [user-file-position]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out
                                                   with-pushed-namespace-values
                                                   with-altered-roots]]
        [midje.internal-ideas.wrapping :only [with-wrapping-target]]
        [midje.util.deprecation :only [deprecate]]
        [midje.ideas.arrow-symbols]
        [clojure.tools.macro :only [macrolet]])
  (:require [midje.data.metaconstant :as metaconstant]
            [clojure.zip :as zip]
            [midje.config :as config]
            [midje.parsing.util.fnref :as fnref]
            [midje.error-handling.exceptions :as exceptions]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit])
  (:import midje.data.metaconstant.Metaconstant))

(defn fake? [form]
  (or (first-named? form "fake") 
      (first-named? form "data-fake")))

(defn tag-as-background-fake [fake]
  `(~@fake :background :background :times (~'range 0)))



(defn #^:private
  statically-disallowed-prerequisite-function
  "To prevent people from mocking functions that Midje itself uses,
   we mostly rely on dynamic checking. But there are functions within
   the dynamic checking code that must also not be replaced. These are
   the ones that are known."
  [some-var]
  (#{#'deref #'assoc} some-var))

(defn raise-disallowed-prerequisite-error [function-name]
  (throw
   (exceptions/user-error
    "You seem to have created a prerequisite for"
    (str (pr-str function-name) " that interferes with that function's use in Midje's")
    (str "own code. To fix, define a function of your own that uses "
         (or (:name (meta function-name)) function-name) ", then")
    "describe that function in a provided clause. For example, instead of this:"
    "  (provided (every? even? ..xs..) => true)"
    "do this:"
    "  (def all-even? (partial every? even?))"
    "  ;; ..."
    "  (provided (all-even? ..xs..) => true)")))



  (defn to-lexical-map-form [ [[fnref & args :as call-form] arrow result & overrides] ]
    ;; The (vec args) keeps something like (...o...) from being
    ;; evaluated as a function call later on. Right approach would
    ;; seem to be '~args. That causes spurious failures. Debug
    ;; someday.
    (when (statically-disallowed-prerequisite-function (fnref/fnref-var-object fnref))
      (raise-disallowed-prerequisite-error (fnref/fnref-var-object fnref)))
    (let [source-details `{:call-form '~call-form
                           :arrow '~arrow
                           :rhs '~(cons result overrides)}]
      `(merge
        (lexical-maps/fake ~call-form ~fnref ~args ~arrow ~result)
        ~source-details
        ~(apply hash-map-duplicates-ok overrides))))
  
