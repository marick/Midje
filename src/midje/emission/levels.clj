(ns ^{:doc "print levels"}
  midje.emission.levels
  (:use [midje.error-handling.exceptions :only [user-error]]
        clojure.pprint))


(def level-names [:print-nothing :print-no-summary :print-normally :print-namespaces :print-facts])
(def levels      [-2        -1           0       1           2])
(def valids (set (concat level-names levels)))
(def names-to-levels (zipmap level-names levels))
(def levels-to-names (zipmap levels level-names))

(defn validate-level! [level-or-name]
  (when-not (valids level-or-name)
    (throw (user-error (str level-or-name " is not a valid :print-level.")))))

(defn normalize [level-or-name]
  (validate-level! level-or-name)
  (if (keyword? level-or-name)
    (names-to-levels level-or-name)
    level-or-name))

(def level-below (comp dec normalize))
(def level-above (comp inc normalize))

