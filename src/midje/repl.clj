(ns ^{:doc "Functions useful when using Midje in the repl or from the command line.
            See `midje-repl-help` for details."}
  midje.repl
  (:use [midje.ideas.reporting.string-format :only [report-strings-summary
                                                    midje-position-string]]
        [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]]
        clojure.pprint)
  (:require midje.sweet
            [midje.clojure-test-facade :as ctf]
            [midje.internal-ideas.compendium :as compendium]
            [midje.util.colorize :as color]
            [clojure.string :as str]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.form-utils :as form]
            [midje.ideas.metadata :as metadata]
            [leiningen.core.project :as project]
            [midje.util.namespace :as namespace]))

(namespace/immigrate-from 'midje.ideas.metadata
                          (map metadata/metadata-function-name
                               metadata/fact-properties))
(when-not (ns-resolve 'user '=>) ; when not already `use`d.
  (namespace/immigrate 'midje.sweet))
(declare forget-facts)



                                ;;; Utilities for printing
;;; TODO: These should migrate elsewhere.

(def ^{:private true} level-names [:nothing :no-summary :normal :namespaces :facts])
(def ^{:private true} levels      [-2        -1           0       1           2])
(def ^{:private true :testable true} names-to-levels (zipmap level-names levels))
(def ^{:private true :testable true} levels-to-names (zipmap levels level-names))

(defn- ^{:testable true} separate-verbosity [args]
  (let [args (replace names-to-levels args)
        [print-level non-levels] (form/separate-by number? args)]
    [(or (first print-level) (names-to-levels :normal))
     non-levels]))

(defn- report-best-fact-name [fact-function print-level]
  (when (>= print-level (names-to-levels :facts))
    (println (color/note
              "Checking "
              (or (fact-name fact-function)
                  (fact-description fact-function)
                  (str "fact at " (midje-position-string
                                   [(fact-file fact-function)
                                    (fact-line fact-function)])))))))

(defn- ^{:testable true} report-summary [print-level]
  (when (> print-level (names-to-levels :no-summary))
    (report-strings-summary (ctf/counters))))

(def ^{:private true} last-namespace-shown (atom nil))

(defn- ^{:testable true} report-changed-namespace [namespace print-level]
  (when (and (>= print-level (names-to-levels :namespaces))
             (not= namespace @last-namespace-shown))
      (println (color/note (str "= Namespace " namespace)))
      (swap! last-namespace-shown (constantly namespace))))


                                ;;; Miscellaneous utilities

(defn- ^{:testable true} forget-past-results []
    (ctf/zero-counters)
    (reset! last-namespace-shown nil))

(defn- check-facts-once-given [fact-functions print-level]
  (forget-past-results)
  (let [run-one (fn [fun]
                  (report-changed-namespace (fact-namespace fun) print-level)
                  (report-best-fact-name fun print-level)
                  (fun))
        results (doall (map run-one fact-functions))]
    (report-summary print-level)
    (every? true? results)))
  

                                ;;; Loading facts from the repl

(defn- ^{:testable true} paths-to-load []
  (try
    (let [project (project/read)]
      (concat (:test-paths project) (:source-paths project)))
    (catch java.io.FileNotFoundException e
      ["test"])))

(defn- ^{:testable true} expand-namespaces [namespaces]
  (mapcat #(if (= \* (last %))
             (namespaces-on-classpath :prefix (apply str (butlast %)))
             [(symbol %)])
          (map str namespaces)))


(defn load-facts*
  "Functional form of `load-facts`."
  [args]
  (let [[print-level args] (separate-verbosity args)
        desired-namespaces (if (empty? args)
                             (mapcat namespaces-in-dir (paths-to-load))
                             (expand-namespaces args))]
    (forget-past-results)
    (doseq [ns desired-namespaces]
      (forget-facts ns)
      (report-changed-namespace ns print-level)
      (require ns :reload))
    (report-summary print-level)
    nil))

(defmacro load-facts 
  "Load given namespaces, described with unquoted symbols, as in:
     (load-facts midje.t-sweet)
   If no namespaces are given, all the namespaces in the project.clj's
   :test-paths and :source-paths will be loaded.
   But if there's no project.clj, all namespaces under \"test\"
   will be loaded.

   A partial namespace ending in a `*` will load all sub-namespaces.
   Example: (load-facts midje.ideas.*)"
  [& args]
  (let [error-fixed (map #(if (form/quoted? %) (second %) %)
                         args)]
    `(load-facts* '~error-fixed)))


                                ;;; Fetching facts

(defn fetch-facts
  "Fetches the facts described by the args.
   If there are no args, fetches the facts in the current namespace (*ns*).
   * if an arg is a namespace or a symbol naming one,
       it fetches the facts in that namespace.
   * (fetch-facts :all) fetches all the facts in all the namespaces.
   * If the argument is a function or a keyword, it is applied to the
       metadata of all known facts. Those that match are returned.
   * If the argument is a string or regexp, any fact whose name
       (typically the doc string) matches the argument is returned.
       Matches are position independent: \"word\" matches \"a word b\"."
  [& args]
  (let [args (if (empty? args) [*ns*] args)]
    (mapcat (fn [arg]
              (cond (string? arg)
                    (filter #(and (fact-name %)
                                  (.contains (fact-name %) arg))
                            (compendium/all-facts<>))
                    
                    (form/regex? arg)
                    (filter #(re-find arg (fact-name %))
                            (compendium/all-facts<>))
                    
                    (= arg :all)
                    (compendium/all-facts<>)
                    
                    (or (fn? arg) (keyword? arg))
                    (filter (comp arg meta) (compendium/all-facts<>))
                    
                    :else
                    (compendium/namespace-facts<> arg)))
            args)))

                                ;;; Forgetting facts


(defn forget-facts 
  "Fetches the facts described by the args.
   If there are no args, forgets the facts in the current namespace (*ns*).
   * if an arg is a namespace or a symbol naming one,
       it forgets the facts in that namespace.
   * (forget-facts :all) forgets all the facts in all the namespaces.
   * If the argument is a function or a keyword, it is applied to the
       metadata of all known facts. Those that match become forgotten..
   * If the argument is a string or regexp, any fact whose name
       (typically the doc string) matches the argument is forgotten.
       Matches are position independent: \"word\" matches \"a word b\"."
  [& args]
  (let [args (if (empty? args) [*ns*] args)]
    (doseq [arg args]
      (cond (= arg :all)
            (compendium/fresh!)

            (or (string? arg)
                (form/regex? arg)
                (fn? arg)
                (keyword? arg))
            (doseq [fact (fetch-facts arg)]
              (compendium/remove-from! fact))
                    
            :else
            (compendium/remove-namespace-facts-from! arg)))))


                                ;;; Checking facts


(defn check-facts
  "Checks the facts described by the args.
   If there are no args, checks the facts in the current namespace (*ns*).
   * if an arg is a namespace or a symbol naming one,
       it checks the facts in that namespace.
   * (check-facts :all) checks all the facts in all the namespaces.
   * If the argument is a function or a keyword, it is applied to the
       metadata of all known facts. Those that match are checked.
   * If the argument is a string or regexp, any fact whose name
       (typically the doc string) matches the argument is checked.
       Matches are position independent: \"word\" matches \"a word b\".
   The return value is `true` if all the facts check out.

   Normally, the output is a single summary line, but there are
   four \"print levels\":
      0 prevents the summary message from printing.
      1 is the normal level.
      2 adds namespaces to the output.
      3 describes each facts as it's checked."
  [& args]
  (let [[print-level args] (separate-verbosity args)
        fact-functions (apply fetch-facts args)]
    (check-facts-once-given fact-functions print-level)))
    

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
   When facts are nested, the entire outer-level fact is rechecked.
   The result is true if the fact checks out.

   Normally, the output is a single summary line, but there are
   four \"print levels\":
      0 prevents the summary message from printing.
      1 is the normal level.
      2 adds the namespace to the output.
      3 describes the fact as it's checked."
  [& args]
  (let [[print-level _] (separate-verbosity args)]
    (check-facts-once-given [(last-fact-checked)] print-level)))

(def ^{:doc "Synonym for `recheck-fact`."} rcf recheck-fact)

(if (ecosystem/running-in-repl?)
  (println (color/note "Run `(midje-repl-help)` for descriptions of Midje repl functions.")))

(defn ^{:doc "Midje repl help"} midje-repl-help []
  (println)
  (println "Here are Midje repl functions. Use `doc` for more info.")
  (println "To control verbosity of output, use print levels defined ")
  (println "in `print-level-help`")
  (println)
  (println "----- Loading facts")
  (println "You load facts by namespace. Namespace names need not be quoted.")
  (println "(load-facts <ns> <ns>...)")
  (println "(load-facts midje.util.*)       ; Load all namespaces below midje.util.")
  (println)
  (println "----- Running facts")
  (println "(check-facts)                   ; check in current namespace")
  (println "(check-facts <ns> <ns>...)      ; check given namespaces")
  (println "(check-facts :all)              ; check all loaded facts")
  (println "(check-facts <pred-or-keyword>) ; <pred> applied to fact metadata")
  (println "(check-facts-named <name>)      ; regex or substring match.")
  (println)
  (println "----- Rerunning facts")
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
  (println "----- Fetching facts")
  (println "Same notation as the `check-facts` family, but with")
  (println "\"fetch\" instead of \"check\"")
  (println)
  (println "The return value is a sequence of functions. You can")
  (println "apply these functions to check the facts.")
  (println)
  (println "To query fact function metadata, use these:")
  (println "-- (fact-name <ff>)                ; result might be nil")
  (println "-- (fact-source <ff>)")
  (println "-- (fact-file <ff>)")
  (println "-- (fact-line <ff>)")
  (println "-- (fact-namespace <ff>)")
  (println "-- (fact-description <ff>)         ; the doc string; might be nil")
  (println)
  )

