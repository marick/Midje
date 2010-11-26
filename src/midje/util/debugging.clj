(ns midje.util.debugging)

(defn nopret [val] val)
(defn pret [val]
  (println val)
  val)

