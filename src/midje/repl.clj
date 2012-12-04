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
            [midje.config :as config]
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

(def ^{:private true} all-keyword-is-not-a-filter
  ;; Prevent namespace arguments from being treated as filters
  (partial = :all))

(defn- fetch-facts-by-namespaces [namespaces]
  (let [namespaces (if (empty? namespaces) [*ns*] namespaces)]
    (mapcat #(if (= % :all)
               (compendium/all-facts<>)
               (compendium/namespace-facts<> %))
            namespaces)))

(defn- check-facts-once-given [fact-functions]
  (levelly/forget-past-results)
  (let [results (doall (map fact/check-one fact-functions))]
    (levelly/report-summary)
    (every? true? results)))

(defn- ^{:testable true} paths-to-load []
  (try
    (let [project (project/read)]
      (concat (:test-paths project) (:source-paths project)))
    (catch java.io.FileNotFoundException e
      ["test"])))

(defn- ^{:testable true} project-namespaces []
  (mapcat namespaces-in-dir (paths-to-load)))

(defn- ^{:testable true} expand-namespaces [namespaces]
  (mapcat #(if (= \* (last %))
             (namespaces-on-classpath :prefix (apply str (butlast %)))
             [(symbol %)])
          (map str namespaces)))

(def ^{:private true} working-set-atom (atom []))
(defn working-set [] @working-set-atom)


                                ;;; Loading facts from the repl
(defn load-facts
  "Load given namespaces, as in:
     (load-facts 'midje.t-sweet 'midje.t-repl)

   A partial namespace ending in a `*` will load all sub-namespaces.
   Example: (load-facts 'midje.ideas.*)

   If the :all argument is given, all the namespaces in the project.clj's
   :test-paths and :source-paths will be loaded.
   But if there's no project.clj, all namespaces under \"test\"
   will be loaded.

   If no namespace arguments are given, the working set is reloaded.
   (See `(doc midje-repl-working-set)`.)

   By default, all facts are loaded from the namespaces. You can, however,
   add further arguments. Only facts matching one or more of the arguments
   are loaded. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed during loading.
   See `(doc midje-print-levels)`."
  [& args]
  (levelly/obeying-print-levels [args args]
    (metadata/obeying-metadata-filters [args args] all-keyword-is-not-a-filter
      (let [desired-namespaces (form/pred-cond args
                                  empty? (working-set)
                                  (partial some #{:all})  (project-namespaces)
                                  :else (expand-namespaces args))]
        (levelly/forget-past-results)
        (doseq [ns desired-namespaces]
          (compendium/remove-namespace-facts-from! ns)
          ;; Following strictly unnecessary, but slightly useful because
          ;; it reports the changed namespace before the first fact loads.
          ;; That way, some error in the fresh namespace won't appear to
          ;; come from the last-loaded namespace.
          (levelly/report-changed-namespace ns)
          (require ns :reload))
        (reset! working-set-atom desired-namespaces)
        (levelly/report-summary)
        nil))))



                                ;;; Fetching loaded facts

(defn fetch-facts
  "Fetch facts that have already been defined, whether by loading
   them from a file or via the repl.

   (fetch-facts *ns* 'midje.t-repl)  -- facts defined in these namespaces
   (fetch-facts :all)                -- all known facts
   (fetch-facts)                     -- facts in the working set
                                        `(doc midje-repl-working-set)`

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are included in the result. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?"

  [& args]
  (let [[filters namespaces]
        (metadata/separate-metadata-filters args all-keyword-is-not-a-filter)

        fact-functions (fetch-facts-by-namespaces namespaces)]
    (filter (metadata/desired-fact-predicate-from filters) fact-functions)))


                              ;;; Forgetting loaded facts

(defn forget-facts 
  "Forget defined facts so that they will not be found by `check-facts`
   or `fetch-facts`.

   (forget-facts)                  -- defined in the current namespace
   (forget-facts *ns* midje.t-repl -- defined in named namespaces
                                      (Names need not be quoted.)
   (forget-facts :all)             -- defined anywhere

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are forgotten. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?"

  [& args]
  (let [[filters namespaces]
        (metadata/separate-metadata-filters args all-keyword-is-not-a-filter)

        namespaces (if (empty? namespaces) [*ns*] namespaces)]
    (if (empty? filters)
      ;; a rare concession to efficiency
      (doseq [ns namespaces]
        (if (= ns :all)
          (compendium/fresh!)
          (compendium/remove-namespace-facts-from! ns)))
      (doseq [fact-function (apply fetch-facts args)]
        (compendium/remove-from! fact-function)))))

    
                                ;;; Checking loaded facts

(def ^{:doc "Check a single fact. Takes as its argument a function such
    as is returned by `last-fact-checked`."}
  check-one-fact fact/check-one)

(defn check-facts
  "Check facts that have already been defined.

   (check-facts)                  -- defined in the current namespace
   (check-facts *ns* midje.t-repl -- defined in named namespaces
                                     (Names need not be quoted.)
   (check-facts :all)             -- defined anywhere

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are checked. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed. See `(doc midje-print-levels)`."
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
   The result is true if the fact checks out.

   The optional argument lets you adjust what's printed.
   See `(print-level-help)` for legal values."
  ([]
     (check-facts-once-given [(last-fact-checked)]))
  ([print-level]
     (config/with-augmented-config {:print-level print-level}
       (recheck-fact))))

(def ^{:doc "Synonym for `recheck-fact`."} rcf recheck-fact)

