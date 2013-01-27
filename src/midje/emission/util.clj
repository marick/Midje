(ns ^{:doc "Utility functions dealing with checking or tranforming forms."}
  midje.emission.util)

(defn midje-position-string [[filename line-num]]
  (format "(%s:%s)" filename line-num))
