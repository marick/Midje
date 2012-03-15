(ns ^{:doc "Midje's special blend of generative-style testing."}
  midje.ideas.formulas
  (:use [midje.util.form-utils :only [first-named? named? pop-docstring]]
        [midje.error-handling.validation-errors :only [simple-report-validation-error 
                                                       validate when-valid]]
        [midje.ideas.prerequisites :only [is-head-of-form-providing-prerequisites?]]
        [midje.ideas.arrows :only [leaf-expect-arrows leaves-contain-arrow?]]
        [midje.ideas.facts :only [future-prefixes]]
        [clojure.walk :only [prewalk]]))

(def ^{:doc "The number of trials generated per formula."
       :dynamic true} 
  *num-trials* 100)   

(set-validator! #'*num-trials* 
  (fn [new-val]
    (if (pos? new-val) 
      true
      (throw (RuntimeException. (str "*num-trials* must be an integer 1 or greater. You tried to set it to: " new-val))))))


(defn shrink [& _args] [])

(defn- formula-fact [docstring body]
  `(midje.sweet/fact ~docstring   
     ~@body :formula :formula-in-progress))

(defmacro shrink-failure-case [docstring binding-names failed-binding-vals body]
  `(loop [shrunk-vectors# (map midje.ideas.formulas/shrink ~failed-binding-vals)]
     (let [cur-shrunks# (map first shrunk-vectors#)]
       (when (and (first cur-shrunks#)
                  (let [~binding-names cur-shrunks#]
                    ~(formula-fact docstring body)))
           (recur (map rest shrunk-vectors#))))))

(defn- deconstruct-formula-args [args]
  (let [[docstring? more-args] (pop-docstring args)
        [opts bindings body] (if (map? (first more-args))
                               [(first more-args) (second more-args) (rest (rest more-args))]
                               [{} (first more-args) (rest more-args)])]
    [docstring? opts bindings body]))

(defmacro formula 
  "ALPHA/EXPERIMENTAL (subject to change) - Generative-style fact macro. 
  
  Ex. (formula \"any two strings concatenated begins with the first\" 
        [a (gen/string) b (gen/string)] 
        (str a b) => (has-prefix a))
        
  Currently, we recommend you use generators from test.generative.generators
  
  The midje.ideas.formulas/*num-trials* dynamic var determines
  how many facts are generated per formula."
  {:arglists '([docstring? bindings & body])}
  [& args]
  (when-valid &form
    (let [[docstring? opts bindings body] (deconstruct-formula-args args)
          fact (formula-fact docstring? body)
          conclusion-signal `(midje.sweet/fact
                               :always-pass midje.sweet/=> :always-pass 
                               :formula :formula-conclude )]

      `(try
         (loop [cnt-down# (if (contains? ~opts :num-trials) (:num-trials ~opts) midje.ideas.formulas/*num-trials*)]
           (when (pos? cnt-down#)
             (let [snd-bindings# ~(vec (take-nth 2 (rest bindings)))
                   ~(vec (take-nth 2 bindings)) snd-bindings#]
               (if ~fact
                 (recur (dec cnt-down#))
                 (shrink-failure-case ~docstring? 
                                      ~(vec (take-nth 2 bindings)) 
                                      snd-bindings# 
                                      ~body)))))
         (finally
           ~conclusion-signal)))))

(def future-formula-variant-names (map #(str % "formula") future-prefixes))

(defn- check-part-of [form]
  (prewalk (fn [form] 
             (if (some (partial first-named? form) ["against-background" "background" "provided"])
                 '() 
                 form)) 
    form))

(defmethod validate "formula" [[_formula_ & args :as form]]
  (cond (not (leaves-contain-arrow? (check-part-of args)))
        (simple-report-validation-error form "There is no expection in your formula form:")
  
        (> (count (leaf-expect-arrows (check-part-of args))) 1)
        (simple-report-validation-error form "There are too many expections in your formula form:")

        (let [[docstring? opts bindings body] (deconstruct-formula-args args)]
          (or (not (vector? bindings))
              (odd? (count bindings))
              (< (count bindings) 2)))
        (simple-report-validation-error form "Formula requires bindings to be an even numbered vector of 2 or more:")

        (some #(and (named? %) (= "background" (name %))) (flatten args))
        (simple-report-validation-error form "background cannot be used inside of formula")
  
        :else 
        args))