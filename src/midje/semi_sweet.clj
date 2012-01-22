;; -*- indent-tabs-mode: nil -*-

;;; This namespace is mainly responsible for converting particular macros
;;; into the arguments used by midje.unprocessed's `expect*`.

(ns ^{:doc "Macros that provide less syntactic sugaring than those 
            from midje.sweet. midje.sweet is built on top of it."}
  midje.semi-sweet
  (:use clojure.test
        midje.internal-ideas.fakes
        midje.internal-ideas.file-position
        [midje.internal-ideas.fact-context :only [within-fact-context]]
        [midje.util debugging form-utils namespace]
        [midje.error-handling validation-errors semi-sweet-validations]
        [midje.error-handling.exceptions :only [user-error]]
        [midje.production-mode]
        [clojure.pprint]))
(immigrate 'midje.unprocessed)
(immigrate 'midje.ideas.arrow-symbols)

;;; Misc

(defn is-semi-sweet-keyword? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect fake not-called data-fake) loc))

;;; Conversions to unprocessed form

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn- #^:tested-private check-for-arrow [arrow]
  (get {=> :check-match
        =expands-to=> :check-match
        =not=> :check-negated-match
        =deny=> :check-negated-match} (name arrow)))

(defmacro unprocessed-check
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect*'."
  [call-form arrow expected-result overrides]
  `(merge
    {:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :desired-check ~(check-for-arrow arrow)
     :expected-result-text-for-failures '~expected-result
     :position (user-file-position)}
    (hash-map-duplicates-ok ~@overrides)))

(letfn [(how-to-handle-check [call-form arrow & _]
          (get {=> :expect*
                =not=> :expect*
                =deny=> :expect*
                =expands-to=> :expect-macro*
                =future=> :report-future-fact} (name arrow)))]

  (defmulti ^{:private true} expect-expansion how-to-handle-check))

(defmethod expect-expansion :expect*
  [call-form arrow expected-result fakes overrides]
  `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
     (expect* check# ~fakes)))

(defmethod expect-expansion :expect-macro*
  [call-form arrow expected-result fakes overrides]
  (let [expanded-macro `(macroexpand-1 '~call-form)
        escaped-expected-result `(quote ~expected-result)]
    (expect-expansion expanded-macro => escaped-expected-result fakes overrides)))

(defmethod expect-expansion :report-future-fact
   [call-form arrow expected-result fakes overrides]
   `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
      (within-fact-context ~(str call-form)  
        (clojure.test/report {:type :future-fact
                              :description (midje.internal-ideas.fact-context/nested-fact-description)
                              :position (:position check#)}))))

;;; Interface: unfinished

(letfn [(unfinished* [names]
          (macro-for [name names]
            `(do
               (defn ~name [& args#]
                 (throw (user-error (str "#'" '~name 
                                      " has no implementation. It's used as a prerequisite in Midje tests."))))
               ;; A reliable way of determining if an `unfinished` function has since been defined.
               (alter-meta! (var ~name) assoc :midje/unfinished-fun ~name))))]

  (defmacro unfinished
    "Defines a list of names as functions that have no implementation yet. They will
     throw Errors if ever called."
    [& names] (unfinished* names))
  
  (defmacro only-mocked 
    "Defines a list of names as functions that have no implementation yet. They will
     throw Errors if ever called.
     DEPRECATED: Prefer `unfinished`."
    {:deprecated "1.3-alpha2"}
    [& names] (unfinished* names)))



;;; Interface: production mode

(defonce
  #^{:doc "True by default.  If set to false, Midje checks are not
     included into production code, whether compiled or loaded."
     :dynamic true}
  *include-midje-checks* true)


;;; Interface: Main macros

(defmacro fake 
  "Creates a fake map that a particular call will be made. When it is made,
   the result is to be returned. Either form may contain bound variables. 
   Example: (let [a 5] (fake (f a) => a))"
  [& forms]
  (when-valid &form (fake* forms)))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  [& forms]
  (when-valid &form (data-fake* forms)))

(defmacro not-called
  "Creates an fake map that a function will not be called.
   Example: (not-called f))
   DEPRECATED: Prefer `:times 0` annotation to `fake`, ex. (provided (f) => 4 :times 0))"
  {:deprecated "1.3-alpha2"}
  [forms]
  (not-called* forms))

(defn- #^:tested-private a-fake? [x]
  (and (seq? x)
       (is-semi-sweet-keyword? (first x))))

(defmacro expect 
  "Run the call form, check that all the mocks defined in the fakes 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument.

   To strip tests from production code, set either clojure.test/*load-tests*
   or midje.semi-sweet/*check* to false."
  [& args]
  (when (user-desires-checking?)
    (valid-let [[call-form arrow expected-result & fakes+overrides] (validate &form)
                [fakes overrides] (separate-by a-fake? fakes+overrides)]
      (when-valid fakes
        (expect-expansion call-form arrow expected-result fakes overrides)))))


