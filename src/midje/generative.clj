(ns midje.generative
  (:require [midje.sweet :refer :all]
            [midje.emission.state :as m-state]
            [midje.emission.api :as emission]
            [midje.parsing.1-to-explicit-form.facts :as parse-facts]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.util.error-handling :as error]

            [midje.emission.plugins.util :as util]
            [midje.emission.api :as api]

            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]

            [clojure.string :as string])
  (:import [java.io StringWriter]))

(defn log-error [seed names values]
  (when (emission/config-above? :print-nothing)
    (util/emit-one-line (str "With seed:\n" seed))
    (util/emit-one-line (str "With generated values: "
                             (util/format-binding-map (zipmap names values))))))
(defn- parse-for-all-form [full-form [binding-form & opts-map-and-checks]]
  (when-not (vector? binding-form)
    (error/report-error full-form "`for-all` must have a vector for its bindings"))

  (when (odd? (count binding-form))
    (error/report-error full-form "`for-all` must have an even number of forms in its binding vector"))

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
    (when-let [extra-keys (and opts? (keys (dissoc opts :num-tests :seed :max-size)))]
      (error/report-error
        full-form (str "unrecognized keys in `for-all` options map: " extra-keys)))
    [prop-names prop-values opts checks]))

(defmacro for-all
  "Check facts using values generated using test.check

  Options
  :seed       used to re-run previous checks
  :max-size   controls the size of generated vlues
  :num-tests  how many times to run the checks

  (for-all 'name \"doc string\"
    [pos-int gen/s-pos-int]
    {:num-tests 10}
    (fact pos-int => integer?))"
  {:arglists '([binding-form & facts]
               [name doc-string binding-form opts-map & facts])}
  [& _]
  (let [[metadata forms] (parse-metadata/separate-metadata &form)
        [prop-names prop-values
         opts checks]           (parse-for-all-form &form forms)
        num-tests               (or (:num-tests opts) 10)
        quick-check-opts        (->> (select-keys opts [:seed :max-size])
                                     (into [])
                                     flatten)]
    `(let [fact-fn# (fn ~prop-names
                      ~(parse-facts/wrap-fact-around-body metadata checks))
           prop#    (prop/for-all* ~prop-values fact-fn#)
           run#     (api/silently (tc/quick-check
                                    num-tests prop# ~@quick-check-opts))]
       (if (:result run#)
         (emission/pass)
         (let [smallest-failing-args (-> run# :shrunk :smallest)]
           (do (apply fact-fn# smallest-failing-args)
               (log-error (:seed run#) '~prop-names smallest-failing-args)))))))
;; TODO PLM:
;; test that fact filtering doesn't apply to nested facts (compare to tabular behavior)
;; show in test that metadata from nested facts isn't lifted to top-level

(defn t []
  (for-all [strictly-pos gen/s-pos-int
            any-integer  gen/int]
           (fact "Summing an integer to a positive integer should be positive? Really?"
             strictly-pos => integer?
             {:x (+ strictly-pos any-integer)} => (contains {:x pos?}))))
  (for-all "bar" [strictly-pos gen/s-pos-int
            any-integer  gen/int]
          (fact "foo"
             strictly-pos => integer?
             {:x (+ strictly-pos any-integer)} => (contains {:x pos?}))
           (fact "Summing an integer to a positive integer should be positive? Really?"
             strictly-pos => integer?
             {:x (+ strictly-pos any-integer)} => (contains {:x pos?})))
