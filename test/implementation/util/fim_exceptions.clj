(ns implementation.util.fim-exceptions
  (:require [midje.util.exceptions :refer :all]
            [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [midje.util :refer :all]))

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

(defn- remove-nrepl-lines [lines]
  (remove #(re-find #">>>refactor_nrepl" %) lines))

(fact
  ;; since midje lines are omitted, there's not much we can check.
  (let [lines (remove-nrepl-lines (friendly-exception-lines (Error. "message") ">>>"))]
    (first lines) => #"Error.*message"
    (re-find #"^>>>" (first lines)) => falsey
    (count (keep #(re-find #">>>implementation.util.fim_exceptions" %) (rest lines))) => (count (rest lines))))

(defn innermost-exception []
  (NullPointerException.))

(defn nested-exception []
  (ex-info "Found a NPE" {:info "wrapped throw of an NPE"} (innermost-exception)))

(defn call-nested-exception []
  (nested-exception))

(fact "exceptions with 'cause' data show the 'cause' stacktrace"
  (let [lines (friendly-exception-lines (call-nested-exception) ">>>")]
    (clojure.string/join "&" lines) => #"(?x)^clojure.lang.ExceptionInfo:\ Found\ a\ NPE\ \{:info\ \"wrapped\ throw\ of\ an\ NPE\"\}
                                            (&>>>.*nested.exception.*)+
                                            (&>>>.*call.nested.exception.*)+
                                             &
                                             &>>>Caused\ by:\ java.lang.NullPointerException
                                            (&>>>.*innermost.exception.*)+
                                            (&>>>.*nested.exception.*)+
                                            (&>>>.*call.nested.exception.*)+"))

(def double-nested-exception
  (ex-info "Exception with a cause chain 2 deep" {:info "2 deep"} (call-nested-exception)))

(fact "exceptions with nested 'cause' data more than 1 level deep, shows all 'cause' stacktraces"
  (let [lines (remove-nrepl-lines (friendly-exception-lines double-nested-exception ">>>"))]
    (clojure.string/join "&" lines) => #"(?x)^clojure.lang.ExceptionInfo:\ Exception\ with\ a\ cause\ chain\ 2\ deep\ \{:info\ \"2\ deep\"\}
                                             &
                                             &>>>Caused\ by:\ clojure.lang.ExceptionInfo:\ Found\ a\ NPE\ \{:info\ \"wrapped\ throw\ of\ an\ NPE\"\}
                                             (&>>>.*nested.exception.*)+
                                            (&>>>.*call.nested.exception.*)+
                                             &
                                             &>>>Caused\ by:\ java.lang.NullPointerException
                                            (&>>>.*innermost.exception.*)+
                                            (&>>>.*nested.exception.*)+
                                            (&>>>.*call.nested.exception.*)+"))
