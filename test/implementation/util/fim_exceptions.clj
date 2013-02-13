(ns implementation.util.fim-exceptions
  (:use [midje.util.exceptions]
        [midje.emission.colorize :only [colorize-choice]]
	      [midje sweet test-util]
        midje.util))
(expose-testables midje.util.exceptions)

(defrecord R [a])

(fact "captured throwables can be recognized"
  (captured-throwable? (captured-throwable (Throwable.))) => truthy
  "and are not fooled by maps or records"
  (captured-throwable? {}) => falsey
  (captured-throwable? (sorted-map :a 3)) => falsey
  (captured-throwable? (R. 1)) => falsey)

;;;;;;

(def clojure-spewage-regexp #"^clojure\..*\(core.clj:\d+\)")

(fact "stacktraces can be fetched as strings"
  (stacktrace-as-strings (Throwable.)) => (contains clojure-spewage-regexp))

(fact "clojure spewage can be removed"
  (let [strings ["clojure.something"
                 "java.something"
                 "midje.something"
                 "other.something"
                 "user$eval19.invoke(NO_SOURCE_FILE:1)"]]
    (without-clojure-strings strings) => ["midje.something" "other.something"])

  "... and midje frames are often considered spewage"
  (let [strings ["clojure.something"
                 "java.something"
                 "midje.something"
                 "other.something"
                 "user$eval19.invoke(NO_SOURCE_FILE:1)"]]
    (without-midje-or-clojure-strings strings) => ["other.something"])

  "... and let us not forget swank spewage"
  (let [strings ["swank.core$eval"]]
    (without-midje-or-clojure-strings strings) => []))

(fact
  ;; since midje lines are omitted, there's not much we can check.
  (let [lines (friendly-exception-lines (Error. "message") ">>>")]
    (first lines) => #"Error.*message"
    (re-find #"^>>>" (first lines)) => falsey
    (count (map #(re-find #">>>implementation.util.t_exceptions" %) (rest lines))) => (count (rest lines))))
