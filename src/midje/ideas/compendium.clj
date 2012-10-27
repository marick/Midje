(ns ^{:doc "A compendium is 'a collection of concise but detailed information
            about a particular subject. The Midje compendium contains
            the currently relevant facts about your program."}
  midje.ideas.compendium)

(def ^:dynamic *parse-time-fact-level* 0)

(defmacro given-possible-fact-nesting [& forms]
  `(binding [*parse-time-fact-level* (inc *parse-time-fact-level*)]
     ~@forms))

(defmacro working-on-nested-facts [& forms]
  ;; Make sure we don't treat this as a top-level fact
  `(binding [*parse-time-fact-level* (+ 2 *parse-time-fact-level*)]
     ~@forms))

(def fact-check-history (atom (constantly true)))

(defn possible-history-recordings [function-symbol]
  (if (= *parse-time-fact-level* 1)
    `((swap! fact-check-history (constantly ~function-symbol)))))
