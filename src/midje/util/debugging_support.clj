(ns midje.util.debugging-support)

(defn nopret [val] val)
(defn pret [val]
  (println val)
  val)

