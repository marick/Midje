(ns midje.util.sequence)

(defn zip [& seqs]
  (apply map list seqs))