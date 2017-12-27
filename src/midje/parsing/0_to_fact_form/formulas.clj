(ns ^{:doc "Midje's special blend of generative-style testing."}
  midje.parsing.0-to-fact-form.formulas
  (:require [clojure.string :refer [join]]
            [clojure.walk :refer [prewalk]]
            [midje.emission.api :as emit]
            [such.types :as types]
            [midje.emission.boundaries :as emission-boundary]
            [midje.emission.state :as state]
            [midje.emission.plugins.silence :as emission-silence]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.recognizing :as recognize]
            [midje.util.pile :as pile]))

;; Formulas work by running up to *num-trials* trials per formula.
(def ^{:doc "The number of trials generated per formula."
       :dynamic true}
  *num-trials* 100)

(set-validator! #'*num-trials*
  (fn [new-val]
    (if (pos? new-val)
      true
      (throw (Error. (str "*num-trials* must be an integer 1 or greater. You tried to set it to: " new-val))))))

;;; Validation

(defn leaf-expect-arrows [nested-form]
  (let [named-form-leaves (map name (filter types/named? (flatten nested-form)))]
    (filter recognize/expect-arrows named-form-leaves)))

(defn leaves-contain-arrow? [nested-form]
  (not (empty? (leaf-expect-arrows nested-form))))

(defn- deconstruct-formula-args [args]
  (let [[docstring? more-args] (pile/pop-docstring args)
        [opts-map [bindings & body]] (pile/pop-opts-map more-args)]
    [docstring? opts-map bindings body]))


(defn- check-part-of [form]
  (prewalk (fn [form]
             (if (some (partial first-named? form) ["against-background" "background" "provided"])
                 '()
                 form))
    form))

(defn- valid-pieces [[_formula_ & args :as form]]
  (let [[docstring? opts-map bindings body] (deconstruct-formula-args args)
        invalid-keys (remove (partial = :num-trials) (keys opts-map))]
    (cond (not (leaves-contain-arrow? (check-part-of args)))
          (error/report-error form "There is no expection in your formula form:")

          (> (count (leaf-expect-arrows (check-part-of args))) 1)
          (error/report-error form "There are too many expections in your formula form:")

          (or (not (vector? bindings))
              (odd? (count bindings))
              (< (count bindings) 2))
          (error/report-error form "Formula requires bindings to be an even numbered vector of 2 or more:")

          (some #(and (types/named? %) (= "background" (name %))) (flatten args))
          (error/report-error form "background cannot be used inside of formula")

          (not (empty? invalid-keys))
          (error/report-error form (format "Invalid keys (%s) in formula's options map. Valid keys are: :num-trials" (join ", " invalid-keys)))

          (and (:num-trials opts-map)
               (not (pos? (:num-trials opts-map))))
          (error/report-error form (str ":num-trials must be an integer 1 or greater. You tried to set it to: " (:num-trials opts-map))))

    [docstring? opts-map bindings body]))


;;; The work

(defn- formula-fact [docstring body]
  `(midje.sweet/fact ~docstring ~@body))


(defmacro around-formula [& body]
  `(do
     ~@body
     (if-let [failure# (last (state/raw-fact-failures))]
       (emit/fail failure#)
       (emit/pass))))

(defmacro formula
  "Generative-style fact macro.

  Ex. (formula \"any two strings concatenated begins with the first\"
        [a (gen/string) b (gen/string)]
        (str a b) => (has-prefix a))

  Currently, we recommend you use generators from test.generative.generators.

  opts-map keys:

     :num-trials - Used to override the number of trials for this formula only.
                   This is higher precedence than *num-trials*
                   Must be set to a number 1 or greater.

  The *num-trials* dynamic var determines
  how many facts are generated per formula."
  {:arglists '([docstring? opts-map? bindings & body])}
  [& _args]
  (error/parse-and-catch-failure &form
   #(let [[docstring? opts-map bindings body] (valid-pieces &form)
          fact (formula-fact docstring? body)]
      `(around-formula
        (loop [num-trials-left# (or (:num-trials ~opts-map) midje.parsing.0-to-fact-form.formulas/*num-trials*)]
          (when (pos? num-trials-left#)
            (let [binding-rightsides# ~(vec (take-nth 2 (rest bindings)))
                  ~(vec (take-nth 2 bindings)) binding-rightsides#
                  success?# (emit/producing-only-raw-fact-failures ~fact)]

              (if success?#
                (recur (dec num-trials-left#))
                success?#))))))))

(defmacro with-num-trials [num-trials & formulas]
  `(binding [midje.parsing.0-to-fact-form.formulas/*num-trials* ~num-trials]
     ~@formulas))

