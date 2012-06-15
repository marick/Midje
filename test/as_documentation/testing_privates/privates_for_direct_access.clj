(ns as-documentation.testing-privates.privates-for-direct-access)

(defn- da-function-without-prerequisite [n] (inc n))

(defn- da-called [n] (dec n))

(defn- da-caller [n] (da-called n))
