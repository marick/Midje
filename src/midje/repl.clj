(ns ^{:doc "Functions useful when using Midje in the repl or from the command line."}
  midje.repl
  (:use [midje.ideas.metadata :only [fact-source]]
        [midje.util.ecosystem :only [fact-namespaces]]
        [midje.ideas.reporting.string-format :only [report-strings-summary]])
  (:require midje.sweet
            [midje.clojure-test-facade :as ctf]
            [midje.internal-ideas.compendium :as compendium]))
  
(defn- check-some-facts [fact-functions]
  (every? true? (doall (map #(%) fact-functions))))

(defn unobtrusive-check-facts
  "With no argument, checks all known facts.
   With arguments (namespaces or symbols), only runs facts
   in those namespaces. Returns true iff all facts check out."
  [& namespaces]
  (check-some-facts
   (if (empty? namespaces)
     (compendium/all-facts<>)
     (mapcat compendium/namespace-facts<> namespaces))))

(defn report-results []
  (report-strings-summary (ctf/counters)))

(defmacro ^{:private true} report-check-group [& body]
  `(do (ctf/zero-counters)
       ~@body
       (report-results)))

                                ;;; Loading facts from the repl

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
  (report-check-group
   (dorun (map #(require % :reload) (apply fact-namespaces args)))))

                                ;;; Facts recorded at load time

(defn check-facts [& args]
  (report-check-group 
   (apply unobtrusive-check-facts args)))

(defn forget-facts
  "After this, `check-facts` does nothing until new facts are defined."
  ([]
     (forget-facts *ns*))
  ([& namespaces]
     (if (= namespaces [:all])
       (compendium/fresh!)
       (dorun (map compendium/remove-namespace-facts-from! namespaces)))
     :done))

(defn fetch-matching-facts
  "Returns a sequence of all facts matching
   the predicate. The predicate is given fact metadata. See
   `check-matching-fact` for midje-supplied metadata"
  [predicate]
  (filter (comp predicate meta)
          (compendium/all-facts<>)))

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
   :midje/source       The original source of the fact."
  [predicate]
  (check-some-facts (fetch-matching-facts predicate)))

(defn all-facts []
  (compendium/all-facts<>))




                                ;;; The history of checking results

(def forget-results ctf/zero-counters)



                                ;;; The history of checked facts

(defn last-fact-checked
  "The last fact or tabular fact that was checked. Only top-level
   facts are recorded, not facts nested within them."
  []
  (compendium/last-fact-checked<>))

(defn source-of-last-fact-checked 
  "Returns the source of the last fact or tabular fact run."
  []
  (fact-source (last-fact-checked)))

(defn recheck-fact 
  "Recheck the last fact or tabular fact that was checked.
   When facts are nested, the entire outer-level fact is rechecked."
  []
  ((last-fact-checked)))

