(ns ^{:doc "Parsing future-facts."}
  midje.parsing.1-to-explicit-form.future-facts
  (:use midje.parsing.util.core)
  (:require [midje.util.pile :as pile]
            [pointer.core :as pointer]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.emission.api :as emit]))

(defn parse [form]
  (let [lineno (reader-line-number form)
        [metadata _] (parse-metadata/separate-metadata form)]
    `(emit/future-fact (nested-facts/descriptions ~(:midje/description metadata))
                       (pointer/line-number-known ~lineno))))


