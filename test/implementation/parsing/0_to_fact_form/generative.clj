(ns implementation.parsing.0-to-fact-form.generative
  (:require [midje.sweet :refer :all]
            [midje.experimental :refer [for-all gen-let]]
            [midje.test-util :refer :all]
            [clojure.test.check.generators :as gen]
            [midje.parsing.0-to-fact-form.generative :as parser]))

(facts "verifying gen-let macro code generation"
  (fact
    (parser/roll-up-bindings
      `([s (gen/return "s")])
      `(gen/return
         {:args     (list s)
          :function fact-fn-sym
          :result   (fact-fn-sym s)}))
    => `(gen/bind
          (gen/return "s")
          (fn
            [s]
            (gen/return
              {:args     (list s)
               :function fact-fn-sym
               :result   (fact-fn-sym s)}))))

  (fact
    (parser/roll-up-bindings
      `([s (gen/return (str i))]
         [i (gen/elements [1 2 3])])
      `(gen/return
         {:args     (list i s)
          :function fact-fn-sym
          :result   (fact-fn-sym i s)}))
    => `(gen/bind
          (gen/elements [1 2 3])
          (fn [i]
            (gen/bind
              (gen/return (str i))
              (fn
                [s]
                (gen/return
                  {:args     (list i s)
                   :function fact-fn-sym
                   :result   (fact-fn-sym i s)}))))))

  (parser/roll-up-bindings
    `([:let [s (str i)]]
       [i (gen/elements [1 2 3])])
    `(gen/return
       {:args     (list i s)
        :function fact-fn-sym
        :result   (fact-fn-sym i s)})))

