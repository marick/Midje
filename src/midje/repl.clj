(ns ^{:doc "Functions useful when using Midje in the repl or from the command line.
            See `midje-repl-help` for details."}
  midje.repl
  (:use [midje.ideas.metadata :only [fact-source]]
        [midje.ideas.reporting.string-format :only [report-strings-summary]])
  (:require midje.sweet
            [midje.clojure-test-facade :as ctf]
            [midje.internal-ideas.compendium :as compendium]
            [midje.util.colorize :as color]
            [clojure.string :as str]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.form-utils :as form]))

(println (color/note "Run `(midje-repl-help)` for a list of functions."))

(defn midje-repl-help []
  (println "Here are Midje repl functions. Use `doc` for more info.")
  (println "Note that `midje.sweet` is not automatically loaded.")
  (println "If you want to define facts in the repl, `use` or `require` it.")
  (println)
  (println "----- Loading facts")
  (println "(load-facts)             ; load facts below \"test\"")
  (println "(load-facts \"<dir>\" \"<dir>\" :prefix \"string\")")
  (println)
  (println "----- Running facts")
  (println "(check-facts)                 ; check in current namespace")
  (println "(check-facts <ns> <ns>...)    ; check given namespaces")
  (println "(check-facts :all)            ; check all loaded facts")
  (println "(check-facts-matching <pred>) ; <pred> matches on fact metadata")
  (println "(check-facts-named <name>)    ; regex or substring match.")
  (println)
  (println "(recheck-fact)                ; Check just-checked fact again.")
  (println "(rcf)                         ; Synonym for above.")
  (println) 
  (println "Note: facts with `:check-only-at-load-time`")
  (println "metadata do not get stored for rerunning.")
  (println)
  (println "----- Forgetting facts")
  (println "Same notation as the `check-facts` family, but with")
  (println "\"forget\" instead of \"check\"")
  (println)
  (println "----- Fetching fact functions")
  (println "Same notation as the `check-facts` family, but with")
  (println "\"fetch\" instead of \"check\"")
  (println)
  (println "Apply a fact function to cause it to check itself.")
  (println "To query fact function metadata, use these:")
  (println "-- (fact-name)                ; might be nil")
  (println "-- (fact-source)")
  (println "-- (fact-file)")
  (println "-- (fact-line)")
  (println "-- (fact-namespace)")
  (println "-- (fact-description)         ; the doc string; might be nil")
  )

                                        ;;; Util  

(defn- report-results []
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
  - include only namespaces whose names begin with \"trad\"
  (load-facts ... :verbose)
  - show all the namespaces being loaded"
  [& args]
  ;; Note: if all the namespaces are loaded in a single `require`,
  ;; Clojure 1.4 (at least) runs out of memory. Moreover,
  ;; (require ns1 ns2 ns3 ... nsN :reload-all) will reload a shared
  ;; dependency N times.
  (let [[verbose? args] (form/separate-by #(= % :verbose) args)]
        (let [namespaces (apply ecosystem/fact-namespaces args)]
          (cond (empty? namespaces)
                (println (color/note "Warning: No matching namespaces."))

                verbose?
                (println (color/note (str "Loading " (str/join ", " namespaces))))

                :else 
                (do
                  (print (color/note (str "Loading " (str/join ", " (take 3 namespaces)))))
                  (when (> (count namespaces) 3)
                    (print (color/note "... \nCall (load-facts :verbose) for a full list of loaded namespaces.")))
                  (println)))
          (report-check-group
           (dorun (map #(require % :reload) namespaces))))))



























  
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

