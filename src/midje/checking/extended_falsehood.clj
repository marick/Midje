(ns ^{:doc "Some failing checks carry additional information."}
  midje.checking.extended-falsehood)

(defn as-data-laden-falsehood [value]
  (vary-meta value assoc :midje/data-laden-falsehood true))

(defn data-laden-falsehood? [value]
  (:midje/data-laden-falsehood (meta value)))

(defn data-laden-falsehood-to-map [value]
  (with-meta value {}))

(defn extended-false? [value]
  (or (not value)
      (data-laden-falsehood? value)))

(defn extended-true? [value]
  (not (extended-false? value)))

(defn user-friendly-falsehood [value]
  (if (data-laden-falsehood? value)
    false
    value))
