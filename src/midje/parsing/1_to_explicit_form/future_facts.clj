(ns ^{:doc "Parsing future-facts."}
  midje.parsing.1-to-explicit-form.future-facts
  (:require [midje.data.nested-facts :as nested-facts]
            [midje.emission.api :as emit]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.util.core :refer :all]
            [midje.util.pile :as pile]
            [pointer.core :as pointer]))

(defn parse [form]
  (let [lineno (reader-line-number form)
        [metadata _] (parse-metadata/separate-metadata form)]
    `(emit/future-fact (nested-facts/descriptions ~(:midje/description metadata))
                       (pointer/line-number-known ~lineno))))


