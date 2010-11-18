(ns midje.util.thread-safe-var-nesting)

(defn push-safely [the-var some-sequence]
  (alter-var-root the-var (partial cons some-sequence)))

(defn pop-safely [the-var]
  (alter-var-root the-var rest))

(defn set-namespace-pseudovariable [key-name newval] 
  (alter-meta! *ns* merge {key-name newval}))
