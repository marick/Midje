(ns midje.repl
  "Functions useful when using Midje in the repl or from the command line.
   See `midje-repl-help` for details."
  (:require [clojure.java.io :as io]
            [midje.checking.facts :as fact-checking]
            [midje.util.exceptions :as exceptions]
            [midje.config :as config]
            [midje.data.compendium :as compendium]
            [midje.data.fact :as fact-data]
            [midje.data.project-state :as project-state]
            [midje.doc :as doc]
            [midje.emission.api :as emit]
            [midje.emission.boundaries :as emission-boundary]
            [midje.emission.colorize :as color]
            [midje.parsing.other.arglists :as parsing]
            midje.sweet
            [midje.util.ecosystem :as ecosystem]
            [midje.util.pile :as pile]
            [midje.util.scheduling :as scheduling]
            [clojure.pprint :refer [cl-format]]
            [such.function-makers :as mkfn]
            [such.shorthand :refer [not-empty?]]
            [such.immigration :as immigrate]))

(fact-data/make-getters *ns* "fact-")

(when (doc/appropriate?)
  (immigrate/import-vars [midje.doc midje-repl])
  (doc/repl-notice))

(when-not (ns-resolve 'user '=>) ; when not already `use`d.
  (immigrate/import-all-vars midje.sweet))



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
  (when-not ((mkfn/pred:any? fn? keyword?) (config/choice :fact-filter))
    (throw (Error. ^String (cl-format nil "The config `:fact-filter` should a function or keyword, not ~A."
                                      (config/choice :fact-filter)))))
  (let [[given-level-seq print-level-to-use args]
          (parsing/separate-print-levels original-args (config/choice :print-level))
        [filters namespaces]
        (parsing/separate-filters args all-keyword-is-not-a-filter)
        filter-function (config/mkfn:fact-filter-predicate filters)]

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



;;; The namespaces in the command arguments affect three different keys:
;;; :given-namespace-args holds the literal values originally given.
;;; That is used to calculate :namespaces-to-use, which is what the
;;; code for the command works with. It is also used to recalculate the
;;; defaults, which will be either :memory-command or both :memory-command
;;; and :disk-command. (Remember, those two keys are both arguments to
;;; choose code to run and the name of remembered values.)

;;; These aren't multimethods because multimethods play poorly with
;;; private status and the reloading that lein-midje does.
(defn- deduce-user-intention-for-memory-command [original-args]
  (let [base (defaulting-args original-args :memory-command)]
    (merge base
           {:namespaces-to-use (:given-namespace-args base)
            :memory-command (:given-namespace-args base)})))

(defn- deduce-user-intention-for-disk-command [original-args]
  (let [base (defaulting-args original-args :disk-command)]
    (merge base
           {:disk-command (:given-namespace-args base)}
           (if (:all? base)
             {:namespaces-to-use (project-state/namespaces)
              :memory-command [:all]}
             (let [expanded (project-state/unglob-partial-namespaces (:given-namespace-args base))]
               {:namespaces-to-use expanded
                :memory-command expanded})))))

(defn- ^{:testable true} deduce-user-intention [true-args namespace-source]
  (if (= namespace-source :memory-command)
    (deduce-user-intention-for-memory-command true-args)
    (deduce-user-intention-for-disk-command true-args)))

;;; A DSLish way of defining intention-obeying functions.

(defmacro ^{:private true} def-obedient-function
  [command-type function-name updatef worker-function docstring]
  `(defn ~function-name
     ~docstring
     [& args#]
     (let [intention# (deduce-user-intention args# ~command-type)
           result# (~worker-function intention#)]
       ;; By doing this after calculating the result, we prevent
       ;; a bad set of arguments from polluting the defaults.
       (~updatef intention# ~command-type)
       result#)))



                                ;;; Loading facts from the repl

;; Loading does not use (require ns :reload). The reason is that :all includes
;; both :test and :source namespaces. If :reload were used, source namespaces
;; would be loaded twice: once indirectly via the tests, and once directly.
;; Instead, we hackishly make Clojure forget that it's loaded certain namespaces.
;; We then load namespaces only if they haven't been loaded before. That will
;; typically mean that source namespace won't be loaded. (Note that this depends
;; on test namespaces preceding source namespaces.)

(defn- forget-certain-namespaces! [namespaces]
  (dosync (alter @#'clojure.core/*loaded-libs* clojure.set/difference (set namespaces))))

(defn- unloaded? [ns]
  (not (contains? @@#'clojure.core/*loaded-libs* ns)))


(def-obedient-function :disk-command load-facts and-update-defaults!
  (fn [intention]
    (let [namespaces (:namespaces-to-use intention)]
      (emission-boundary/around-namespace-stream namespaces
                                                 {:print-level (:print-level intention)
                                                  :fact-filter (:filter-function intention)}
        (forget-certain-namespaces! namespaces)
        (doseq [ns namespaces :when (unloaded? ns)]
          (compendium/remove-namespace-facts-from! ns)
          ;; Following strictly unnecessary, but slightly useful because
          ;; it reports the changed namespace before the first fact loads.
          ;; That way, some error in the fresh namespace won't appear to
          ;; come from the last-loaded namespace.
          (emit/possible-new-namespace ns)
          (try (require ns :reload)
               (catch Exception e
                 (println (color/fail "LOAD FAILURE for " ns))
                 (if (config/choice :pretty-print)
                   (println (exceptions/format-exception e))
                   (println (.getMessage e)))))))))
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
  (let [facts (if (:all? intention)
                (compendium/all-facts<>)
                (mapcat compendium/namespace-facts<> (:namespaces-to-use intention)))]
    (filter (:filter-function intention) facts)))

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
   reused. The filters are also reused, unless they're overridden
   with explicit arguments.
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
  check-one-fact fact-checking/check-one)

(defn- ^{:testable true} check-facts-once-given [facts]
  (emission-boundary/around-fact-stream facts config/no-overrides
    (doseq [f facts] (check-one-fact f))))


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

(defonce ^{:doc "Stores last exception encountered in autotesting"}
  *me nil)

(defn- on-require-failure [the-ns ^Throwable throwable]
  (println (color/fail "LOAD FAILURE for " the-ns))
  (if (config/choice :pretty-print)
    (println (exceptions/format-exception throwable))
    (println (.getMessage throwable)))
  (emit/fail-silently) ; to make sure last line shows a failure.
  (when (config/running-in-repl?)
    (when (re-find #"ould not locate.*classpath" (.getMessage throwable))
      (println "+ This error indicates that the first argument to `ns` doesn't match the file path.")
      (println "+ That breaks autotest's file tracking (even for files without errors).")
      (println "+ After you fix the problem, rerun `autotest`.")
      (println))
    (println "The exception has been stored in #'*me, so `(pst *me)` will show the stack trace.")
    (alter-var-root #'*me (constantly throwable))))


(declare autotest-options)

(defn- namespace-stream-checker [namespaces a-sort-of-continuation]
  (let [chosen-fact-filter-predicate (config/mkfn:fact-filter-predicate (:fact-filters (autotest-options)))]
    (emission-boundary/around-namespace-stream namespaces {:fact-filter chosen-fact-filter-predicate}
      (println (color/note "\n======================================================================"))
      (println (color/note "Loading " (pr-str namespaces)))
      ;; It's not really a continuation because it's wrapped inside `around-namespace-stream`.
      (a-sort-of-continuation))))

(defn autotest-default-dirs
  "These are the directories that will be watched if you type
   `autotest` with no arguments."
  []
  (ecosystem/leiningen-paths))

(defn- exists-on-filesystem? [file-or-dir-name]
  (let [file-or-dir (io/file file-or-dir-name)]
    (or (.isDirectory file-or-dir)
        (.isFile file-or-dir))))

(defn- complained-about-missing-on-filesystem? [candidate-strings]
  (let [missing (remove exists-on-filesystem? candidate-strings)]
    (when-not (empty? missing)
      (println (color/fail (cl-format nil "~[~;This is not a directory or file:~:;These are not directories or files:~] ~{~S~^, ~}."
                                      (count missing)
                                      missing))))
    (not-empty? missing)))

(defonce ^{:private true}
  autotest-options-atom
  (atom {:interval 500
         :files (autotest-default-dirs)
         :fact-filters []

         ;; These options aren't really settable. They serve to
         ;; decouple the checking of facts from the handling of
         ;; project state and tracking of file changes.
         :on-require-failure on-require-failure
         :namespace-stream-checker namespace-stream-checker}))

(defn autotest-options
  "If you want a peek at how autotest is controlled, look here."
  [] @autotest-options-atom)

(defn set-autotest-option!
  "Set autotest options without starting autotesting."
  [key value]
  (swap! autotest-options-atom assoc key value))

(defn autotest
  "`autotest` checks frequently for changed files. It reloads those files
  and all files that depend on them. Since test files depend on source files,
  that typically results in facts being reloaded and checked.

  By default, `autotest` monitors all the files in the
  project.clj's :source-paths and :test-paths. To change the
  default, give the `:files` argument:

     (autotest :files \"test/midje/util\" \"src/midje/util\")
     (autotest :files \"test/midje/util/t_unify.clj\" \"src/midje/util/unify.clj\")

  This is useful in large projects. Note that `autotest` doesn't follow
  dependencies outside the given directories. If `test/midje/util/A`
  depends on `some-untracked-dir/B` that depends on `src/midje/util/C`,
  a change to `C` will not cause `A` to be reloaded.

  You can revert back to the project.clj versions like this:

    (autotest :all)

  You can filter the facts checked by giving more arguments following
  a `:filter` or `:filters` keyword:

    (autotest :filter :integration)

  Facts matching any of the arguments are the ones that are
  checked. The arguments can be of these types:

  :keyword      -- Does the metadata have a truthy value for the keyword?
  \"string\"    -- Does the fact's name contain the given string?
  #\"regex\"    -- Does any part of the fact's name match the regex?
  a function    -- Does the function return a truthy value when given
                   the fact's metadata?

  `autotest` can take special keyword arguments:

     (autotest :stop)   ; stop checking
     (autotest :pause)  ; pause checking
     (autotest :resume) ; continue after a pause

  `autotest` can also be given an `:interval` argument to control how often
  it checks for modified files. The argument is in milliseconds:

      (autotest :interval 1000) ; check each second.

  The default is to check twice each second.
  "
  [& args]
  (letfn [(start-periodic-check []
            (scheduling/schedule :autotest
                                 (project-state/mkfn:react-to-changes (autotest-options))
                                 (:interval (autotest-options))))]

    ;; Note that stopping and pausing, which seeming different to user, actually do
    ;; exactly the same thing.
    (let [option ((parsing/make-option-arglist-parser [:files :file :dirs :dir]
                                                      [:interval :each]
                                                      [:filters :filter]
                                                      [:stop :pause] [:resume] [:all])
                  args)]

      (cond (not (empty? (:true-args option)))
            (println (color/fail "Did you mean to put the arguments after `:files`?"))

            (:stop? option)
            (scheduling/stop :autotest)

            (:resume? option)
            (start-periodic-check)

            (empty? args)
            (try
              (project-state/load-everything (autotest-options))
              (start-periodic-check)
            (catch Throwable t
              (autotest :stop)
              (println (color/fail "Because failures in the initial load break autotest's dependency tracking,"))
              (println (color/fail "autotest has been cancelled."))
              (println (color/fail (.getMessage t)))))



            (and (:files? option)
                 (complained-about-missing-on-filesystem? (:files-args option)))
            :oops

            :else
            (do
              (when (:all? option) (set-autotest-option! :files (autotest-default-dirs)))
              (when (:files? option) (set-autotest-option! :files (:files-args option)))
              (when (:filters? option) (set-autotest-option! :fact-filters (:filters-args option)))
              (when (:interval? option) (set-autotest-option! :interval (first (:interval-args option))))
              (autotest)))))
  true)

