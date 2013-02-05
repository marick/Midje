(ns ^{:doc "An intermediate stage in the compilation of prerequisites."}
  midje.parsing.2-to-lexical-maps.fakes
  (:use midje.clojure.core
        midje.parsing.util.core
        midje.parsing.util.zip
        [midje.parsing.arrow-symbols])
  (:require [midje.parsing.util.fnref :as fnref]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.emission.api :as emit]))

(defn fake? [form]
  (or (first-named? form "fake") 
      (first-named? form "data-fake")))

(defn tag-as-background-fake [fake]
  `(~@fake :background :background :times (~'range 0)))

(defn- compiler-will-inline-fn? [var]
  (contains? (meta var) :inline))

(defn- exposed-testable? [var]
  (contains? (meta var) :testable))

(defn #^:private
  statically-disallowed-prerequisite-function-set
  "To prevent people from mocking functions that Midje itself uses,
   we mostly rely on dynamic checking. But there are functions within
   the dynamic checking code that must also not be replaced. These are
   the ones that are known."
  [some-var]
  (#{#'deref #'assoc} some-var))

(defn assert-right-shape! [[_fake_ funcall & _ :as form]]
  (when-not (or (list? funcall)
                (seq? funcall))
    (error/report-error form
                        "The left-hand side of a prerequisite must look like a function call or metaconstant."
                        (cl-format nil "`~S` doesn't." funcall))))


(defn disallowed-function-failure-lines [function-var]
  ["You seem to have created a prerequisite for"
   (str (pr-str function-var) " that interferes with that function's use in Midje's")
   (str "own code. To fix, define a function of your own that uses "
        (:name (meta function-var)) ", then")
   "describe that function in a provided clause. For example, instead of this:"
   "  (provided (every? even? ..xs..) => true)"
   "do this:"
   "  (def all-even? (partial every? even?))"
   "  ;; ..."
   "  (provided (all-even? ..xs..) => true)"])

(defn valid-pieces [[_ [fnref & args :as call-form] arrow result & overrides]]
  (let [actual-var (fnref/fnref-var-object fnref)]
    (cond (compiler-will-inline-fn? actual-var)
          (error/report-error call-form
                              (cl-format nil "You cannot override the function `~S`: it is inlined by the Clojure compiler." actual-var))

          (exposed-testable? actual-var)
          (error/report-error call-form
                              "A prerequisite cannot use a symbol exposed via `expose-testables` or `testable-privates`."
                              (cl-format nil "Instead, use the var directly: #'~S/~S"
                                         (-> actual-var meta :ns ns-name)
                                         fnref))
          ('#{truthy falsey} result)
          (error/report-error call-form
                              (cl-format nil "... ~S ~A ~S" call-form arrow result)
                              "Do you really want a checker on the right-hand side of a prerequisite?"
                              "It's easy to use `truthy` when you meant `true`, etc.")

          (statically-disallowed-prerequisite-function-set actual-var)
          (apply error/report-error call-form (disallowed-function-failure-lines actual-var))))
  [call-form fnref args arrow result overrides])

(defn assert-valid! [form]
  (assert-right-shape! form)
  (valid-pieces form))

(defn to-lexical-map-form [a-list]
  (assert-right-shape! a-list)
  (apply lexical-maps/fake (valid-pieces a-list)))
    
