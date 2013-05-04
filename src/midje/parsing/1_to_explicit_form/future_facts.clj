(ns ^{:doc "Parsing future-facts."}
  midje.parsing.1-to-explicit-form.future-facts
  (:use midje.parsing.util.core)
  (:require [midje.util.pile :as pile]
            [midje.parsing.util.file-position :as position]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.parsing.1-to-explicit-form.background :as background]
            [midje.emission.api :as emit]))

(defn parse [form]
  (let [lineno (reader-line-number form)
        [metadata _] (parse-metadata/separate-metadata form)]
    (background/note-fact!)
    `(emit/future-fact (nested-facts/descriptions ~(:midje/description metadata))
                       (position/line-number-known ~lineno))))


