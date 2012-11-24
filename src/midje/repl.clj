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
(declare forget-facts)



                                ;;; Utilities

(defn- report-summary []
  (report-strings-summary (ctf/counters)))

(defn- separate-options [possible-options args]
  (let [[options non-options] (form/separate-by (set possible-options) args)]
    [(set options) non-options]))

(defn- print-fact-position [fact-function]
  (println (color/note
            "Checking "
            (or (fact-name fact-function)
               (fact-description fact-function)
               (str "fact at " (midje-position-string
                                [(fact-file fact-function)
                                 (fact-line fact-function)]))))))



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


(defn load-facts
  "Load given namespaces, described with symbols or strings.
   If none are given, all the namespaces in the project.clj's
   :test-paths and :source-paths will be loaded.
   But If there's no project.clj, all namespaces under \"test\"
   will be loaded.

   A partial namespace ending in a `*` will load all sub-namespaces.
   Example: (load-facts 'rose.ideas.*)
   Including the `:verbose` keyword causes each namespace to be printed
   as it's loaded."
  [ & args]
  (let [[options args] (separate-options [:verbose] args)
        desired-namespaces (if (empty? args)
                             (mapcat namespaces-in-dir (paths-to-load))
                             (expand-namespaces args))]
    (ctf/zero-counters)
    (doseq [ns desired-namespaces]
      (forget-facts ns)
      (when (options :verbose) (println (color/note (str ns))))
      (require ns :reload))
    (report-summary)
    nil))



                                ;;; Fetching facts

(defn fetch-facts [& args]
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


(defn forget-facts [& args]
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

(defn- check-facts-once-given [fact-functions options]
  (ctf/zero-counters)
  (let [results (doall (map (fn [fun]
                              (when (options :verbose)
                                (print-fact-position fun))
                              (fun))
                            fact-functions))]
    (when-not (options :no-summary)
      (report-strings-summary (ctf/counters)))
    (every? true? results)))
  

(defn check-facts
  [& args]
  (let [[options args] (separate-options [:no-summary :verbose] args)
        fact-functions (apply fetch-facts args)]
    (check-facts-once-given fact-functions options)))
    

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
  [& args]
  (let [[options _] (separate-options [:no-summary :verbose] args)]
    (check-facts-once-given [(last-fact-checked)] options)))

(def rcf recheck-fact)

(if (ecosystem/running-in-repl?)
  (println (color/note "Run `(midje-repl-help)` for a list of functions.")))

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
  (println "(check-facts)                   ; check in current namespace")
  (println "(check-facts <ns> <ns>...)      ; check given namespaces")
  (println "(check-facts :all)              ; check all loaded facts")
  (println "(check-facts <pred-or-keyword>) ; <pred> applied to fact metadata")
  (println "(check-facts-named <name>)      ; regex or substring match.")
  (println)
  (println "Add a :verbose option to print a fact's name or location")
  (println "before it's run. Add :no-summary to prevent an end-of-run")
  (println "summary from being printed.")
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

