(ns ^{:doc "A stupid whimsy that I must drag along with me forever"}
  midje.parsing.util.future-variants
  (:require [midje.parsing.util.core :refer :all]
            [midje.util.pile :as pile]))

(def future-prefixes ["future-"
                      "pending-"
                      "incipient-"
                      "antiterminologicaldisintactitudinarian-"])

(def future-fact-variant-names (for [prefix future-prefixes
                                     fact-or-facts ["fact" "facts"]]
                                 (str prefix fact-or-facts)))

(defmacro generate-future-fact-variants []
  (pile/macro-for [name future-fact-variant-names]
    `(defmacro ~(symbol name)
       "Fact that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (midje.parsing.1-to-explicit-form.future-facts/parse ~'&form))))

(def future-formula-variant-names (map #(str % "formula") future-prefixes))

(defmacro generate-future-formula-variants []
  (pile/macro-for [name future-formula-variant-names]
    `(defmacro ~(symbol name)
       "ALPHA/EXPERIMENTAL (subject to change)
        Formula that will not be run. Generates 'WORK TO DO' report output as a reminder."
       {:arglists '([& forms])}
       [& forms#]
       (midje.parsing.1-to-explicit-form.future-facts/parse ~'&form))))

