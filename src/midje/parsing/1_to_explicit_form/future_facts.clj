(ns ^{:doc "Parsing future-facts."}
  midje.parsing.1-to-explicit-form.future-facts
  (:use midje.parsing.util.core)
  (:require [midje.util.pile :as pile]
            [midje.parsing.util.file-position :as position]
            [midje.data.nested-facts :as nested-facts]
            [midje.parsing.1-to-explicit-form.metadata :as parse-metadata]
            [midje.emission.api :as emit]))

(def future-prefixes ["future-" 
                      "pending-" 
                      "incipient-" 
                      "antiterminologicaldisintactitudinarian-"])

(def future-fact-variant-names (for [prefix future-prefixes
                                     fact-or-facts ["fact" "facts"]]
                                 (str prefix fact-or-facts)))

(defmacro generate-variants []
  (pile/macro-for [name future-fact-variant-names]
    `(defmacro ~(symbol name)
       "Fact that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (parse ~'&form))))

(defn future-fact? [form]
  (some (partial first-named? form) future-fact-variant-names ))

(defn parse [form]
  (let [lineno (reader-line-number form)
        [metadata _] (parse-metadata/separate-metadata form)]
    `(emit/future-fact (nested-facts/descriptions ~(:midje/description metadata))
                       (position/line-number-known ~lineno))))


