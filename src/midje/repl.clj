(ns ^{:doc "Functions useful when using Midje in the repl or from the command line."}
  midje.repl
  (:use [midje.ideas.metadata :only [fact-source]]
        [midje.util.ecosystem :only [fact-namespaces]]
        [midje.ideas.reporting.string-format :only [report-strings-summary]])
  (:require clojure.test
            [midje.ideas.rerunning-facts :as rerun]))


;;; The last fact can be rechecked

(defn last-fact-checked
  "The last fact or tabular fact that was checked. Only top-level
   facts are recorded, not facts nested within them."
  []
  (rerun/last-fact-function-run))

(defn source-of-last-fact-checked 
  "Returns the source of the last fact or tabular fact run."
  []
  (fact-source (last-fact-checked)))

(defn recheck-fact 
  "Recheck the last fact or tabular fact that was checked.
   When facts are nested, the entire outer-level fact is rechecked."
  []
  ((last-fact-checked)))

(defn check-facts
  "With no argument, checks all known facts.
   With arguments (namespaces or symbols), only runs facts
   in those namespaces. Returns true iff all facts check out."
  [& namespaces]
  (rerun/check-some-facts
   (if (empty? namespaces)
     (rerun/compendium-contents)
     (mapcat rerun/namespace-facts namespaces))))

(defn forget-facts
  "After this, `check-facts` does nothing until new facts are defined."
  ([]
     (rerun/forget-facts-in-namespace *ns*))
  ([& namespaces]
     (if (= namespaces [:all])
       (rerun/reset-compendium)
       (dorun (map rerun/forget-facts-in-namespace namespaces)))))

(defn fetch-matching-facts
  "Returns a sequence of all facts matching
   the predicate. The predicate is given fact metadata. See
   `check-matching-fact` for midje-supplied metadata"
  [predicate]
  (filter (comp predicate meta) (rerun/compendium-contents)))

(defn check-matching-facts
  "The function is given each fact's metadata.
   It checks each fact that matches the predicate. In addition to
   user-supplied metadata, facts will also have this metadata:

   :midje/description  The fact's outermost doc-string, if given.
   :midje/name         The *string* name of the fact. 
                       Derived from the symbol name of the fact, if given.
                       Otherwise, the doc string, if given.
                       Otherwise nothing.

   :midje/namespace    The namespace containing the fact.
   :midje/file         The file containing the fact.
   :midje/line         The line number of the fact's first line.
   :midje/true-name    A symbol that's a unique identifier.
   :midje/source       The original source of the fact."
  [predicate]
  (rerun/check-some-facts (fetch-matching-facts predicate)))

(defn load-facts
  "Load facts from all namespaces within given directories.
  (load-facts \"dir1\" \"dir2\")
  - from namespaces like dir1.this.that
  (load-facts)
  - load facts from all namespaces under \"test\"
  (load-facts ... :prefix \"trad\")
  - include only namespaces whose names begin with \"trad\""
  [& args]
  ;; Note: if all the namespaces are loaded in a single `require`,
  ;; Clojure 1.4 (at least) runs out of memory. Moreover,
  ;; (require ns1 ns2 ns3 ... nsN :reload-all) will reload a shared
  ;; dependency N times.
  (dorun (map #(require % :reload) (apply fact-namespaces args))))




;;; TODO - the workflow these imply needs testing


(defn forget-results []
  (alter-var-root (var clojure.test/*report-counters*)
                  (fn [_#] (ref clojure.test/*initial-report-counters*))))

(defn report-results []
  (report-strings-summary @clojure.test/*report-counters*))
