(ns ^{:doc "Macros that provide less syntactic sugaring than those 
            from midje.sweet. midje.sweet is built on top of it."}
  midje.semi-sweet
  (:use midje.clojure.core
        midje.data.prerequisite-state
        [midje.util debugging] 
        [midje.emission.deprecation :only [deprecate]]
       [midje.util.exceptions :only [user-error]]
        [midje.util.ecosystem :only [line-separator]]
        midje.production-mode
        [clojure.algo.monads :only [domonad]]
        [clojure.string :only [join]])
  (:require [midje.util.pile :as pile]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit]
            midje.checking.checkables
            [midje.parsing.util.fnref :as fnref]
            [midje.parsing.2-to-lexical-maps.expects :as parse-expects]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.parsing.2-to-lexical-maps.data-fakes :as parse-data-fakes]))

;;; Once upon a time, this fine represented a lower level user interface that midje.sweet was
;;; written in terms of. It has mostly been phased out, but isn't all gone yet. 

(defmacro fake 
  "Creates a fake map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  {:arglists '([call-form arrow result & overrides])}
  [& _]
  (parse-fakes/to-lexical-map-form &form))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  {:arglists '([metaconstant arrow contained & overrides])}
  [& _]
  (parse-data-fakes/to-lexical-map-form &form))




