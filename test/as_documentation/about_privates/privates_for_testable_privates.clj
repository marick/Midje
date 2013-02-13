(ns as-documentation.about-privates.privates-for-testable-privates)

(defn- tp-function-without-prerequisite [n] (inc n))

(defn- tp-called [n] (dec n))

(defn- tp-caller [n] (tp-called n))
