(ns midje.generative
  (:require [midje.sweet :refer :all]
            [midje.emission.state :as m-state]
            [midje.emission.api :as emission]

            [midje.emission.plugins.util :as util]

            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]

            [clojure.string :as string])
  (:import [java.io StringWriter]))

(defmacro silently [& forms]
  `(let [output-counters-before# (m-state/output-counters)
         writer# (new StringWriter)]
     (binding [clojure.test/*test-out* writer#]
       (let [result# (do ~@forms)]
         (m-state/set-output-counters! output-counters-before#)
         result#))))

(defn log-error [names values]
  (when (emission/config-above? :print-nothing)
    (util/emit-one-line (str "With generated values: "
                             (util/format-binding-map (zipmap names values))))))

(def ^:dynamic *midje-generative-runs* 10)

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro for-all
  "Check facts using values generated using test.check

  Options
  :seed       used to re-run previous checks
  :max-size   controls the size of generated vlues
  :num-tests  how many times to run the checks

  (for-all [pos-int gen/s-pos-int] {:num-tests 10} (fact pos-int => integer?))"
  {:arglists '([])}
  [binding-forms & opts-map-and-checks]
  (assert-args
     (vector? binding-forms) "a vector for its binding"
     (even? (count binding-forms)) "an even number of forms in binding vector")
  (let [bindings    (partition 2 binding-forms)
        prop-names  (mapv first bindings)
        prop-values (mapv second bindings)
        has-opts?   (-> opts-map-and-checks first map?)
        opts        (if has-opts?
                      (-> opts-map-and-checks
                          first
                          (select-keys [:seed :max-size])
                          (->> (into []))
                          flatten)
                      [])
        checks      (if has-opts?
                      (rest opts-map-and-checks)
                      opts-map-and-checks)]
    `(let [fact-fn# (fn ~prop-names (fact ~@checks))
           prop#    (prop/for-all* ~prop-values fact-fn#)
           run#     (silently (tc/quick-check *midje-generative-runs* prop# ~@opts))]
       (if (:result run#)
         (emission/pass)
         (do (log-error '~prop-names (-> run# :shrunk :smallest))
             (apply fact-fn# (-> run# :shrunk :smallest)))))))

(defn t []
  (for-all [strictly-pos gen/s-pos-int
            any-integer  gen/int]
           (fact "Summing an integer to a positive integer should be positive? Really?"
             strictly-pos => integer?
             {:x (+ strictly-pos any-integer)} => (contains {:x pos?}))))

(comment
  (midje.emission.state/reset-output-counters!)
  (midje.emission.state/output-counters)
  )
