(ns ^{:doc "Functions useful when using Midje in the repl or from the command line.
            See `midje-repl-help` for details."}
  midje.repl
  (:use clojure.pprint)
  (:require midje.sweet
            [midje.doc :as doc]
            [midje.config :as config]
            [midje.ideas.facts :as fact]
            [midje.ideas.reporting.levels :as levelly]
            [midje.ideas.metadata :as metadata]
            [midje.internal-ideas.compendium :as compendium]
            [midje.internal-ideas.project-state :as project-state]
            [midje.util.form-utils :as form]
            [midje.util.colorize :as color]
            [midje.util.ecosystem :as ecosystem]
            [midje.util.scheduling :as scheduling]
            [midje.util.namespace :as namespace]))

(when (and (config/running-in-repl?) (ecosystem/clojure-1-2-X?))
  (println (color/fail "The Midje repl tools don't work on Clojure 1.2.X")))
(ecosystem/when-1-3+

(namespace/immigrate-from 'midje.ideas.metadata
                          (map metadata/metadata-function-name
                               metadata/fact-properties))

(when (doc/appropriate?)
  (namespace/immigrate-from 'midje.doc doc/for-repl)
  (doc/repl-notice))

(when-not (ns-resolve 'user '=>) ; when not already `use`d.
  (namespace/immigrate 'midje.sweet))



                                ;;; User Intentions

;; We go to some trouble to have a mostly-consistent interface to the functions
;; {load,fetch,check,forget}-facts. The arguments to the functions are bundled
;; together into a map that controls:
;;
;; Which namespaces should be used.
;; Which output should be printed
;; Whether facts in namespaces should be filtered.
;; Whether the default case for future uses should be changed.


;; The `:all` keyword means "do this function to all namespaces". 
(defn- do-to-all? [args]
  (boolean (some #{:all} args)))
;; It has to be distinguished from arguments that set up filters.
(def ^{:private true} all-keyword-is-not-a-filter
  (partial = :all))


;; Defaulting. The basic rule for defaults is that if you don't
;; mention a namespace in a command, the previous print levels,
;; filters, and namespaces are reused. If you do, all of them are
;; replaced.

;; Further, loading has a different way of working with namespaces
;; than the other functions. (For example, it supports wildcards.)
;; Moreover, loading affects the defaults for other functions, but
;; other functions don't affect loading defaults.

;; So there are two sets of defaults: one for `:disk-commands` and one
;; for `:memory-commands`. As a trick to eliminate some if statements,
;; the respective "command types" are used to store the namespace
;; defaults. 

(def ^{:private true, :testable true}
  default-args (atom {:memory-command
                      {:memory-command [:all]
                       :given-filter-args []
                       :given-level-args []}
                      :disk-command
                      {:disk-command [:all]
                       :given-filter-args []
                       :given-level-args []}}))

(defn- default-as-needed [command-type arg-type given]
  (if (empty? given)
    (get-in @default-args [command-type arg-type])
    given))

(defn- update-one-default! [intention command-type]
  (swap! default-args
         assoc command-type
         (select-keys intention
                      [command-type :given-filter-args :given-level-args])))

;; The funny name is because it's used in a DSL below. 
(defn- ^{:testable true} and-update-defaults! [intention command-type]
  (update-one-default! intention :memory-command)
  (when (= command-type :disk-command)
    (update-one-default! intention :disk-command)))
(defn- without-updating-defaults [intention scope] "do nothing")



;; This function makes user intentions explicit.

(defn- ^{:testable true} defaulting-args [original-args command-type]
  (let [[given-level-seq print-level-to-use args]
          (levelly/separate-print-levels original-args)
        [filters filter-function namespaces]
        (metadata/separate-filters args all-keyword-is-not-a-filter)]

    (if (empty? namespaces)
      (defaulting-args
        (mapcat (partial default-as-needed command-type)
                [command-type :given-filter-args :given-level-args]
                [namespaces filters given-level-seq])
        command-type)
      {:given-namespace-args namespaces
       :given-filter-args filters
       :given-level-args given-level-seq
       
       :all? (do-to-all? namespaces)
       :print-level print-level-to-use
       :filter-function filter-function})))


(defmulti deduce-user-intention
  "This has to be public because multimethods are second-class."
  (fn [_ namespace-source] namespace-source))

;;; The namespaces in the command arguments affect three different keys:
;;; :given-namespace-args holds the literal values originally given.
;;; That is used to calculate :namespaces-to-use, which is what the
;;; code for the command works with. It is also used to recalculate the
;;; defaults, which will be either :memory-command or both :memory-command
;;; and :disk-command. (Remember, those two keys are both arguments to 
;;; choose code to run and the name of remembered values.)
  
(defmethod deduce-user-intention :memory-command [original-args _]
  (let [base (defaulting-args original-args :memory-command)]
    (merge base
           {:namespaces-to-use (:given-namespace-args base)
            :memory-command (:given-namespace-args base)})))

(defmethod deduce-user-intention :disk-command [original-args _]
  (let [base (defaulting-args original-args :disk-command)]
    (merge base
           {:disk-command (:given-namespace-args base)}
           (if (:all? base)
             {:namespaces-to-use (project-state/namespaces)
              :memory-command [:all]}
             (let [expanded (project-state/unglob-partial-namespaces (:given-namespace-args base))]
               {:namespaces-to-use expanded
                :memory-command expanded})))))

;;; A DSLish way of defining intention-obeying functions.

(defmacro ^{:private true} def-obedient-function
  [command-type function-name update worker-function docstring]
  `(defn ~function-name
     ~docstring
     [& args#]
     (let [intention# (deduce-user-intention args# ~command-type)
           result# (~worker-function intention#)]
       ;; By doing this after calculating the result, we prevent
       ;; a bad set of arguments from polluting the defaults.
       (~update intention# ~command-type)
       result#)))



                                ;;; Loading facts from the repl

(def-obedient-function :disk-command load-facts and-update-defaults!
  (fn [intention]
    (config/with-augmented-config {:print-level (:print-level intention)
                                   :desired-fact? (:filter-function intention)}
      (levelly/forget-past-results)
      (doseq [ns (:namespaces-to-use intention)]
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
   are loaded. The filter arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed during loading.
   See `(doc midje-print-levels)`.

   If the call doesn't mention any namespaces, the ones from
   the previous `load-facts` are reused. The filters and print-levels
   are also reused, unless they're overridden with explicit arguments.
   "
)

                                ;;; Fetching loaded facts

;; An independent function because it's not just used by fetch-facts.
(defn- fetch-intended-facts [intention]
  (let [fact-functions (if (:all? intention)
                         (compendium/all-facts<>)
                         (mapcat compendium/namespace-facts<> (:namespaces-to-use intention)))]
    (filter (:filter-function intention) fact-functions)))

(def-obedient-function :memory-command fetch-facts and-update-defaults!
  fetch-intended-facts
  "Fetch facts that have already been defined, whether by loading
   them from a file or via the repl.

   (fetch-facts *ns* 'midje.t-repl)  -- facts defined in these namespaces
   (fetch-facts :all)                -- all known facts
   (fetch-facts)                     -- reuse previous arguments

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are included in the result. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   If the call doesn't mention any namespaces, the ones from the
   previous `load-facts`, `check-facts`, or `fetch-facts` are
   reused. The filters and print-levels are also reused, unless
   they're overridden with explicit arguments.
   "
)
     

                              ;;; Forgetting loaded facts

(def-obedient-function :memory-command forget-facts without-updating-defaults
  (fn [intention]
    ;; a rare concession to efficiency
    (cond (and (empty? (:given-filter-args intention)) (:all? intention))
          (compendium/fresh!)
          
          (empty? (:given-filter-args intention))
          (dorun (map compendium/remove-namespace-facts-from!
                      (:namespaces-to-use intention)))
          
          :else
          (dorun (map compendium/remove-from!
                      (fetch-intended-facts intention)))))
  "Forget defined facts so that they will not be found by `check-facts`
   or `fetch-facts`.

   (forget-facts *ns* midje.t-repl -- defined in named namespaces
   (forget-facts :all)             -- defined anywhere
   (forget-facts)                  -- forget facts worked on by most
                                      recent `check-facts`, `load-facts`,
                                      or `fetch-facts`.

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are forgotten. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   Filters from the previous command are reused unless they're overridden.
   "
  )
     

    
                                ;;; Checking loaded facts

(def ^{:doc "Check a single fact. Takes as its argument a function such
    as is returned by `last-fact-checked`."}
  check-one-fact fact/check-one)

(defn- ^{:testable true} check-facts-once-given [fact-functions]
  (levelly/forget-past-results)
  (let [results (doall (map check-one-fact fact-functions))]
    (levelly/report-summary)
    (if (empty? results)
      nil
      (every? true? results))))

(def-obedient-function :memory-command check-facts and-update-defaults!
  (fn [intention]
    (config/with-augmented-config {:print-level (:print-level intention)}
      (check-facts-once-given (fetch-intended-facts intention))))
  "Check facts that have already been defined.

   (check-facts *ns* midje.t-repl) -- defined in named namespaces
   (check-facts :all)              -- defined anywhere
   (check-facts)                   -- check same facts again

   You can further filter the facts by giving more arguments. Facts matching
   any of the arguments are the ones that are checked. The arguments are:

   :keyword      -- Does the metadata have a truthy value for the keyword?
   \"string\"    -- Does the fact's name contain the given string? 
   #\"regex\"    -- Does any part of the fact's name match the regex?
   a function    -- Does the function return a truthy value when given
                    the fact's metadata?

   In addition, you can adjust what's printed. See `(doc midje-print-levels)`.

   If the call doesn't mention any namespaces, the ones from the
   previous `load-facts`, `check-facts`, or `fetch-facts` are
   reused. The filters and print-levels are also reused, unless
   they're overridden with explicit arguments.
   "
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


                                ;;; Autotest

(def ^{:private true, :testable true}
  autotest-interval (atom 500))

(defmulti autotest
  "`autotest` checks frequently for changed files. It reloads those files
  and all files that depend on them. Since test files depend on source files,
  that typically results in facts being reloaded and checked. 

  `autotest` monitors all the files in the project.clj's :source-paths
  and :test-paths. If you don't have a project.clj file, don't use
  `autotest`.

  `autotest` can be given an :each argument to control how often
  it checks for modified files. The argument is in milliseconds:

      (autotest :each 1000) ; check each second.

  `autotest` can take special keyword arguments:

     (autotest :stop)   ; stop checking
     (autotest :pause)  ; pause checking
     (autotest :resume) ; continue after a pause
  "
  (fn [& args]
    (or ((set args) :stop)
        ((set args) :pause)
        ((set args) :resume)
        (pos? (count args)))))

(defmethod autotest :default []
  (autotest :each @autotest-interval))

(defmethod autotest true [& args]
  (let [options (apply hash-map args)]
    (swap! autotest-interval #(or (:each options) %))
    (println)
    (project-state/load-everything)
    (autotest :resume)))

(defmethod autotest :resume [_]
  (scheduling/schedule :autotest project-state/load-changed @autotest-interval)
  true)

(defmethod autotest :stop [_]
  (scheduling/stop :autotest)
  true)

(defmethod autotest :pause [_]
  (autotest :stop))
)
