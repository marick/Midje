(ns ^{:doc "Functions useful when using Midje in the repl or from the command line.
            See `midje-repl-help` for details."}
  midje.repl
  (:use [bultitude.core :only [namespaces-in-dir namespaces-on-classpath]]
        clojure.pprint)
  (:require midje.sweet
            [midje.ideas.facts :as fact]
            [midje.internal-ideas.compendium :as compendium]
            [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.metadata :as metadata]
            [midje.doc :as doc]
            [leiningen.core.project :as project]
            [midje.util.form-utils :as form]
            [midje.util.namespace :as namespace]))

(namespace/immigrate-from 'midje.ideas.metadata
                          (map metadata/metadata-function-name
                               metadata/fact-properties))

(when (doc/appropriate?)
  (namespace/immigrate-from 'midje.doc doc/for-repl)
  (doc/repl-notice))

(when-not (ns-resolve 'user '=>) ; when not already `use`d.
  (namespace/immigrate 'midje.sweet))



                                ;;; Miscellaneous utilities


(defn- check-facts-once-given [fact-functions]
  (levelly/forget-past-results)
  (let [results (doall (map fact/check-one fact-functions))]
    (levelly/report-summary)
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


(declare forget-facts)
(defn load-facts*
  "Functional form of `load-facts`."
  [args]
  (levelly/obeying-print-levels [args args]
    (let [desired-namespaces (if (empty? args)
                               (mapcat namespaces-in-dir (paths-to-load))
                               (expand-namespaces args))]
    (levelly/forget-past-results)
    (doseq [ns desired-namespaces]
      (forget-facts ns)
      ;; Following strictly unnecessary, but slightly useful because
      ;; it reports the changed namespace before the first fact loads.
      ;; That way, some error in the fresh namespace won't appear to
      ;; come from the last-loaded namespace.
      (levelly/report-changed-namespace ns)
      (require ns :reload))
    (levelly/report-summary)
    nil)))

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
              (cond (metadata/name-matcher? arg)
                    (filter (comp (partial metadata/name-matches? arg) meta)
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
       metadata of all known facts. Those that match are forgotten.
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

(def ^{:doc "Check a single fact. Takes as its argument a function such
    as is returned by `last-fact-checked`."}
  check-one-fact fact/check-one)

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

   Note: Multiple arguments have the effect of concatenating the
   matches for each one. That can lead to duplicates. I'll fix it
   if anyone ever cares."
  [& args]
  (levelly/obeying-print-levels [args args]
    (let [fact-functions (apply fetch-facts args)]
    (check-facts-once-given fact-functions))))
    

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
   The result is true if the fact checks out."
  [& args]
  (levelly/obeying-print-levels [args args]
    (check-facts-once-given [(last-fact-checked)])))

(def ^{:doc "Synonym for `recheck-fact`."} rcf recheck-fact)

