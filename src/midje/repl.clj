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

(defn- do-to-all? [args]
  (boolean (some #{:all} args)))

(defn- check-facts-once-given [fact-functions]
  (levelly/forget-past-results)
  (let [results (doall (map fact/check-one fact-functions))]
    (levelly/report-summary)
    (every? true? results)))

(defn- ^{:testable true} project-directories []
  (try
    (let [project (project/read)]
      (concat (:test-paths project) (:source-paths project)))
    (catch java.io.FileNotFoundException e
      ["test"])))

(defn- ^{:testable true} project-namespaces []
  (mapcat namespaces-in-dir (project-directories)))

(defn- ^{:testable true} unglob-partial-namespaces [namespaces]
  (mapcat #(if (= \* (last %))
             (namespaces-on-classpath :prefix (apply str (butlast %)))
             [(symbol %)])
          (map str namespaces)))

(defmulti deduce-user-intention
  "This has to be public because multimethods are second-class."
  (fn [_ type] type))
(defmethod deduce-user-intention :for-in-memory-facts [original-args type]
  (let [[print-level args]
        (levelly/separate-print-levels original-args)
        [filters filter-function args]
        (metadata/separate-filters args all-keyword-is-not-a-filter)]
    {:all? (do-to-all? args)
     :namespaces args
     :original-args original-args,
     :print-level print-level
     :filters filters
     :filter-function filter-function
     :fetch-default-args original-args}))
(defmethod deduce-user-intention :for-facts-anywhere [original-args type]
  (let [base (deduce-user-intention original-args :for-in-memory-facts)]
    (merge base
           {:load-default-args (:original-args base)}
           (if (:all? base)
             {:namespaces (project-namespaces)}
             (let [expanded (unglob-partial-namespaces (:namespaces base))]
               {:namespaces expanded
                :fetch-default-args (concat expanded
                                            (:filters base)
                                            [(:print-level base)])})))))




                                ;;; Loading facts from the repl


(def ^{:private true, :testable true} load-default-args (atom [:all]))
(def ^{:private true, :testable true} fetch-default-args (atom [:all]))

(defn- either-args [given default]
  (if (empty? given) default given))

(defn- update-defaults! [intention]
  (reset! fetch-default-args (:fetch-default-args intention))
  (when (contains? intention :load-default-args)
    (reset! load-default-args (:load-default-args intention))))
(def ^{:private true} and-update-defaults! update-defaults!)
(def ^{:private true} without-updating-defaults (fn [intention] ))

(def ^{:private true} default-atoms
  {:for-in-memory-facts 'fetch-default-args
   :for-facts-anywhere 'load-default-args})


(defmacro ^{:private true} def-obedient-function
  [function-name scope update worker-function docstring]
  (let [default-atom (scope default-atoms)]
    `(defn ~function-name
       ~docstring
       [& args#]
       (let [intention# (deduce-user-intention (either-args args# @~default-atom)
                                               ~scope)]
         (~update intention#)
         (~worker-function intention#)))))

(def-obedient-function load-facts :for-facts-anywhere and-update-defaults!
  (fn [intention]
    (config/with-augmented-config {:print-level (:print-level intention)
                                   :desired-fact? (:filter-function intention)}
      (levelly/forget-past-results)
      (doseq [ns (:namespaces intention)]
        (compendium/remove-namespace-facts-from! ns)
        ;; Following strictly unnecessary, but slightly useful because
        ;; it reports the changed namespace before the first fact loads.
        ;; That way, some error in the fresh namespace won't appear to
        ;; come from the last-loaded namespace.
        (levelly/report-changed-namespace ns)
        (require ns :reload))
      (levelly/report-summary)
      nil))
  "Load given namespaces, as in:
     (load-facts 'midje.t-sweet 'midje.t-repl)

   A partial namespace ending in a `*` will load all sub-namespaces.
   Example: (load-facts 'midje.ideas.*)

   If the :all argument is given, all the namespaces in the project.clj's
   :test-paths and :source-paths will be loaded.
   But if there's no project.clj, all namespaces under \"test\"
   will be loaded.

   By default, all facts are loaded from the namespaces. You can, however,
   add further arguments. Only facts matching one or more of the arguments
   are loaded. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed during loading.
   See `(doc midje-print-levels)`.

   If `load-facts` is given no arguments, it reuses the previous arguments."
)

                                ;;; Fetching loaded facts

;; An independent function because it's not just used by fetch-facts.
(defn- fetch-intended-facts [intention]
  (let [fact-functions (if (:all? intention)
                         (compendium/all-facts<>)
                         (mapcat compendium/namespace-facts<> (:namespaces intention)))]
    (filter (:filter-function intention) fact-functions)))

(def-obedient-function fetch-facts :for-in-memory-facts and-update-defaults!
  fetch-intended-facts
  "Fetch facts that have already been defined, whether by loading
   them from a file or via the repl.

   (fetch-facts *ns* 'midje.t-repl)  -- facts defined in these namespaces
   (fetch-facts :all)                -- all known facts

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are included in the result. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   If no arguments are given, it reuses the arguments from the most
   recent `check-facts`, `fetch-facts`, or `load-facts`."
)
     

                              ;;; Forgetting loaded facts

(def-obedient-function forget-facts :for-in-memory-facts without-updating-defaults
  (fn [intention]
    ;; a rare concession to efficiency
    (cond (and (empty? (:filters intention)) (:all? intention))
          (compendium/fresh!)
          
          (empty? (:filters intention))
          (dorun (map compendium/remove-namespace-facts-from!
                      (:namespaces intention)))
          
          :else
          (dorun (map compendium/remove-from!
                      (fetch-intended-facts intention)))))
  "Forget defined facts so that they will not be found by `check-facts`
   or `fetch-facts`.

   (forget-facts *ns* midje.t-repl -- defined in named namespaces
   (forget-facts :all)             -- defined anywhere
   (forget-facts)                  -- forget facts worked on by most
                                      recent `check-facts` or `load-facts`.

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are forgotten. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?"
  )
     

    
                                ;;; Checking loaded facts

(def ^{:doc "Check a single fact. Takes as its argument a function such
    as is returned by `last-fact-checked`."}
  check-one-fact fact/check-one)


(def-obedient-function check-facts :for-in-memory-facts and-update-defaults!
  (fn [intention]
    (config/with-augmented-config {:print-level (:print-level intention)}
      (check-facts-once-given (fetch-intended-facts intention))))
  "Check facts that have already been defined.

   (check-facts *ns* midje.t-repl -- defined in named namespaces
   (check-facts :all)             -- defined anywhere

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are checked. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed. See `(doc midje-print-levels)`.

   If no arguments are given, it reuses the arguments from the most
   recent `check-facts`, `fetch-facts`, or `load-facts`."
  )
    


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

