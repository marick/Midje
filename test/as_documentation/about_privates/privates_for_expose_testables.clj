(ns as-documentation.about-privates.privates-for-expose-testables)

(defn- ^{:testable true}
  et-function-without-prerequisite [n]
    (inc n))

(defn- ^{:testable true}
  et-called [n]
    (dec n))

(defn- ^{:testable true}
  et-caller [n]
    (et-called n))
