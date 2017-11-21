(ns midje.experimental
  (:require [midje.parsing.0-to-fact-form.generative :as parse-generative]
            [pointer.core :as pointer]))

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
  (pointer/set-fallback-line-number-from &form)
  (parse-generative/parse-for-all &form))
