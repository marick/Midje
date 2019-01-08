(ns midje.experimental
  (:require [midje.parsing.0-to-fact-form.generative :as parse-generative]
            [pointer.core :as pointer]))

(defmacro for-all
  "Check facts using values generated using test.check generators,
   as long as the generators are independent of each other (see gen-let
   for a more general alternative)

  Options
  :seed       used to re-run previous checks
  :max-size   controls the size of generated vlues
  :num-tests  how many times to run the checks

  (for-all \"doc string\"
       [pos-int gen/s-pos-int
        neg-int gen/s-neg-int]
       {:num-tests 500}
       (fact (+ pos-int neg-int) => #(<= neg-int % pos-int)))"
  {:arglists '([binding-form & facts]
               [name doc-string binding-form opts-map & facts])}
  [& _]
  (pointer/set-fallback-line-number-from &form)
  (parse-generative/parse-for-all &form))

(defmacro gen-let
  "Check facts using values generated using test.check generators,
   allowing generators to be composed together. gen-let is strictly
   more powerful than for-all, but exhibits worse performance and
   test case shrinking behavior, as it desugars to a chan of `bind`
   expressions.

   Options
   :seed       used to re-run previous checks
   :max-size   controls the size of generated vlues
   :num-tests  how many times to run the checks

   (fact \"doc string\"
     (gen-let [i  gen/s-pos-int
               is (gen/vector gen/int i)
               e  (gen/elements is)]
       (count is) => i
       (some #{e} is) => e))

   Intermediate computations can be done with the `:let` keyword as in:
   (gen-let [i  (gen/choose 0 9)
             :let [s  (str i)
                   s2 (str s s)]
             xs (gen/elements [s s2])]
       (->> xs seq (map #(Character/getNumericValue %)) (some #{i})) => i)"
  {:arglists '([binding-form & facts]
               [name doc-string binding-form opts-map & facts])}
  [& _]
  (pointer/set-fallback-line-number-from &form)
  (parse-generative/parse-gen-let &form))
