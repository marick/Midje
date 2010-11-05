(ns midje.util.thread-safe-var-nesting)

(defn push-safely [the-var fakes]
  (alter-var-root the-var (partial cons fakes)))

(defn pop-safely [the-var]
  (alter-var-root the-var rest))
