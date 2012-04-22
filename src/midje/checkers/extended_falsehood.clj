(ns ^{:doc "Some failing checks carry additional information."}
  midje.checkers.extended-falsehood)

(defn as-data-laden-falsehood [value] ; was as-chatty-falsehood 
  (vary-meta value assoc :midje/data-laden-falsehood true))

(defn data-laden-falsehood? [value]   ; chatty-checker-falsehood
  (:midje/data-laden-falsehood (meta value)))

(defn data-laden-falsehood-to-map [value]
  (with-meta value {}))

(defn extended-false? [value]    ; was chattily-false
  (or (not value)
      (data-laden-falsehood? value)))

