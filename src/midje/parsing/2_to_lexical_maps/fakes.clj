(ns ^{:doc "An intermediate stage in the compilation of prerequisites."}
  midje.parsing.2-to-lexical-maps.fakes
  (:require [clojure.pprint :refer [cl-format]]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.lexical-maps :as lexical-maps]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.fnref :as fnref]))

(defn tag-as-background-fake [fake line-override]
  `(~@fake :position ~line-override :background :background :times (range 0)))

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
  (#{#'deref #'assoc #'list? #'print #'println} some-var))

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

(defn- valid-pieces [[_ [fnref & args :as call-form] arrow result & overrides]]
  (let [actual-var (memoize (partial fnref/resolved-to-actual-var-object fnref))]
    (cond (keyword? fnref)
          (error/report-error call-form
                              "The first value in a prerequisite's call form has to be a var or a symbol."
                              (cl-format nil "~S starts with a keyword." call-form))

          (compiler-will-inline-fn? (actual-var))
          (error/report-error call-form
                              (cl-format nil "You cannot override the function `~S`: it is inlined by the Clojure compiler." (actual-var)))

          (and (symbol? fnref) (exposed-testable? (actual-var)))
          (error/report-error call-form
                              "A prerequisite cannot use a symbol exposed via `expose-testables` or `testable-privates`."
                              (cl-format nil "Instead, use the var directly: #'~S/~S"
                                         (-> (actual-var) meta :ns ns-name)
                                         fnref))
          ('#{truthy falsey} result)
          (error/report-error call-form
                              (cl-format nil "... ~S ~A ~S" call-form arrow result)
                              "Do you really want a checker on the right-hand side of a prerequisite?"
                              "It's easy to use `truthy` when you meant `true`, etc.")

          (statically-disallowed-prerequisite-function-set (actual-var))
          (apply error/report-error call-form (disallowed-function-failure-lines (actual-var)))))
  [call-form fnref args arrow result overrides])

(defn assert-valid! [form]
  (assert-right-shape! form)
  (valid-pieces form))

(defn to-lexical-map-form [a-list]
  (assert-right-shape! a-list)
  (apply lexical-maps/fake (valid-pieces a-list)))

(defmacro fake
  "Creates a fake map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables.
   Example: (let [a 5] (fake (f a) => a))"
  {:arglists '([call-form arrow result & overrides])}
  [& _]
  (to-lexical-map-form &form))

