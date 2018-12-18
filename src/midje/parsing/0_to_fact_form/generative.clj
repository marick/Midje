(ns ^{:doc "Check multiple facts given clojure.check generated values, failing values are shrunk before being shown."}
  midje.parsing.0-to-fact-form.generative
  (:require [midje.emission.api :as emission]
            [midje.emission.colorize :as color]
            [midje.emission.plugins.util :as util]
            [midje.emission.state :as state]
            [midje.parsing.1-to-explicit-form.facts :as parse-facts]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.util.error-handling :as error]
            [clojure.test.check :as tc]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as generators]
            [clojure.test.check.generators :as gen]))

(def default-num-tests 10)

(defn- log-qc-info [seed names values]
  (emission/info [""
                  (color/note "quick-check seed:")
                  seed
                  ""
                  (color/note "quick-check shrunken failing values:")
                  (util/attractively-stringified-value (zipmap names values))]))

(defn- form-name [full-form]
  (-> full-form first name))

(defn- parse-for-all-form [full-form [binding-form & opts-map-and-checks]]
  (when-not (vector? binding-form)
    (error/report-error full-form (format "`%s` must have a vector for its bindings" (form-name full-form))))

  (when (empty? binding-form)
    (error/report-error full-form (format "`%s` cannot have an empty binding vector" (form-name full-form))))

  (when (odd? (count binding-form))
    (error/report-error
      full-form (format "`%s` must have an even number of forms in its binding vector" (form-name full-form))))

  (let [bindings    (partition 2 binding-form)
        prop-names  (mapv first bindings)
        prop-values (mapv second bindings)
        opts?       (-> opts-map-and-checks first map?)
        opts        (if opts?
                      (first opts-map-and-checks)
                      {})
        checks      (if opts?
                      (rest opts-map-and-checks)
                      opts-map-and-checks)]
    (when (and (contains? opts :num-tests)
               (not (pos? (:num-tests opts))))
      (error/report-error
        full-form (format ":num-tests `%s` option must be greater than 0: %d"
                          (form-name full-form)
                          (:num-tests opts))))

    (when-let [extra-keys (and opts? (keys (dissoc opts :num-tests :seed :max-size)))]
      (error/report-error
        full-form (format "unrecognized keys in `%s` options map: %s" (form-name full-form) extra-keys)))
    [prop-names prop-values opts checks]))

(defn run-with-smallest [fact-fn prop-names run-result]
  (let [smallest-failing-args (-> run-result :shrunk :smallest)]
    (do (apply fact-fn smallest-failing-args)
        (log-qc-info (:seed run-result) prop-names smallest-failing-args))))

(defn run-for-all [quickcheck-args]
  (let [passes  (atom nil)
        run     (emission/silently
                  (let [result (apply tc/quick-check quickcheck-args)]
                    (reset! passes (state/output-counters:midje-passes))
                            result))]
    [run @passes]))

(def quick-check-opt-keys [:seed :max-size])

(defn options [opts-map-form]
  {:num-tests (get opts-map-form :num-tests default-num-tests)
   :quick-check-opts (->> (select-keys opts-map-form quick-check-opt-keys)
                          (into [])
                          flatten)})

(defn- build-for-all-parser [form]
  (fn []
    (let [[metadata forms] (parse-metadata/separate-metadata form)
          [prop-names prop-values opts checks] (parse-for-all-form form forms)
          {:keys [num-tests quick-check-opts]} (options opts)
          fact-fn-name (gensym "fact-fn")]
      `(let [~fact-fn-name       (fn ~prop-names
                              ~(parse-facts/wrap-fact-around-body
                                 metadata checks))
             prop#          (prop/for-all* ~prop-values ~fact-fn-name)
             [run# passes#] (run-for-all (list ~num-tests
                                               prop#
                                               ~@quick-check-opts))]
         (if (:pass? run#)
           (dotimes [_# (/ passes# ~num-tests)]
             (emission/pass))
           (run-with-smallest ~fact-fn-name '~prop-names run#))
         (:pass? run#)))))

(declare roll-up-bindings)

(defn bind-expr [gen-name gen-expr inner-body other-bindings]
  (roll-up-bindings
    other-bindings
    (list `generators/bind
          gen-expr
          (list `fn [gen-name] inner-body))))

(defn let-expr [fmap-bindings inner-body other-bindings]
  (roll-up-bindings
    other-bindings
    `(let ~fmap-bindings
       ~inner-body)))

(defn roll-up-bindings [bindings inner-body]
  (let [[fst-binding & rst-bindings] bindings
        [lhs rhs] fst-binding]
    (cond
      (empty? bindings) inner-body
      (= :let lhs)      (let-expr rhs inner-body rst-bindings)
      :else             (bind-expr lhs rhs inner-body rst-bindings))))

(defn roll-up [bindings gen-names check-fn-name]
  (roll-up-bindings (reverse bindings)
                    `(gen/return {:function ~check-fn-name
                                  :result   (~check-fn-name ~@gen-names)
                                  :args     (list ~@gen-names)})))

(defn variables [bindings]
  (->> bindings
       (mapcat (fn [[lhs rhs]]
                 (cond (= :let lhs)  (->> rhs (partition 2) (map first))
                       :else [lhs])))
       vec))

(defn- build-gen-let-parser [form]
  (fn []
    (let [[metadata forms] (parse-metadata/separate-metadata form)
          [prop-names prop-values
           opts checks] (parse-for-all-form form forms)
          {:keys [num-tests quick-check-opts]} (options opts)
          fact-fn-name     (gensym "fact-fn")
          bindings         (map vector prop-names prop-values)
          vars             (variables bindings)
          prop             (roll-up bindings vars fact-fn-name)]
      `(let [~fact-fn-name  (fn ~vars
                              ~(parse-facts/wrap-fact-around-body
                                 metadata checks))
             [run# passes#] (run-for-all (list ~num-tests
                                               ~prop
                                               ~@quick-check-opts))]
         (if (:pass? run#)
           (dotimes [_# (/ passes# ~num-tests)]
             (emission/pass))
           (run-with-smallest ~fact-fn-name '~vars run#))
         (:pass? run#)))))

(defn parse-for-all [form]
  (error/parse-and-catch-failure form (build-for-all-parser form)))

(defn parse-gen-let [form]
  (error/parse-and-catch-failure form (build-gen-let-parser form)))

