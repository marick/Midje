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
            [midje.error-handling.exceptions :as exceptions]
            [midje.emission.api :as emit])
  (:import midje.data.metaconstant.Metaconstant))

(defn fake? [form]
  (or (first-named? form "fake") 
      (first-named? form "data-fake")))

(defn tag-as-background-fake [fake]
  `(~@fake :background :background :times (~'range 0)))


