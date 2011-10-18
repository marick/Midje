;; -*- indent-tabs-mode: nil -*-

;;; This namespace is mainly responsible for converting particular macros
;;; into the arguments used by midje.unprocessed's `expect*`.

(ns midje.semi-sweet
  (:use clojure.test
        midje.internal-ideas.fakes
        midje.internal-ideas.file-position
        [midje.util debugging form-utils namespace]
        [midje.error-handling monadic semi-sweet-errors]
        [midje.util.exceptions :only [user-error]]
        [midje.production-mode]
        [clojure.pprint]
        [midje.util.old-clojure-contrib.def :only [defmacro-]]
        [midje.util.old-clojure-contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.unprocessed)
(immigrate 'midje.ideas.arrow-symbols)

;;; Misc

(defn is-semi-sweet-keyword? [loc]
  (matches-symbols-in-semi-sweet-or-sweet-ns? '(expect fake not-called data-fake) loc))

(defn- fakes-and-overrides [form]
  (let [fake? #(and (seq? %)
                    (is-semi-sweet-keyword? (first %)))]
    (separate-by fake? form)))

;;; Conversions to unprocessed form

;; I want to use resolve() to compare calls to fake, rather than the string
;; value of the symbol, but for some reason when the tests run, *ns* is User,
;; rather than midje.semi_sweet_test. Since 'fake' is used only in the latter,
;; the tests fail.
;;
;; FURTHERMORE, I wanted to use set operations to check for fake and not-called,
;; but those fail for reasons I don't understand. Bah.
(defn- check-for-arrow [arrow]
  (get {=> :check-match
        =not=> :check-negated-match
        =deny=> :check-negated-match} (name arrow)))

(defmacro unprocessed-check [call-form arrow expected-result overrides]
  "Creates a map that contains a function-ized version of the form being 
   tested, an expected result, and the file position to refer to in case of 
   failure. See 'expect*'."
  `(merge
    {:function-under-test (fn [] ~call-form)
     :expected-result ~expected-result
     :desired-check ~(check-for-arrow arrow)
     :expected-result-text-for-failures '~expected-result
     :position (user-file-position)}
    (hash-map-duplicates-ok ~@overrides)))

(defn- handling-of-check-part [call-form arrow & _]
  (get {=> :expect*
        =not=> :expect*
        =deny=> :expect*
        =future=> :report-future-fact} (name arrow)))

(defmulti ^{:private true} expect-expansion handling-of-check-part)

(defmethod expect-expansion :expect*
  [call-form arrow expected-result fakes overrides]
  `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
     (expect* check# (vector ~@fakes))))

(defmethod expect-expansion :report-future-fact
   [call-form arrow expected-result fakes overrides]
  `(let [check# (unprocessed-check ~call-form ~arrow ~expected-result ~overrides)]
    (clojure.test/report {:type :future-fact
                          :description ~(str call-form " ")
                          :position (:position check#)})))

;;; Interface: unfinished

(defn- unfinished* [names]
  (let [declarations (map (fn [name] 
                              `(defn ~name [& args#] 
                                 (throw (user-error (str "#'" '~name " has no implementation. It's used as a prerequisite in Midje tests.")))))
                          names)]
    `(do ~@declarations)))

(defmacro unfinished
  "Defines a list of names as functions that have no implementation yet. They will
   throw Errors if ever called."
  [& names] (unfinished* names))

(defmacro only-mocked 
  "Defines a list of names as functions that have no implementation yet. They will
   throw Errors if ever called.
   DEPRECATED: Prefer `unfinished`."
  [& names] (unfinished* names))



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
  (error-let [ rest (validate &form)]
    (fake* rest)))

(defmacro data-fake
  "Creates a fake map that's used to associate key/value pairs with a metaconstant"
  [& forms]
  (error-let [ rest (validate &form)]
    (data-fake* rest)))

(defmacro not-called
  "Creates an fake map that a function will not be called.
   Example: (not-called f))
   DEPRECATED: Prefer `:times 0` or `:never` annotation to `fake`."
  [var-sym & overrides]
  (make-fake-map var-sym
                 `{:call-text-for-failures (str '~var-sym " was called.")
                   :result-supplier (fn [] nil)
                   :type :not-called}
                 overrides))

(defmacro expect 
  "Run the call form, check that all the mocks defined in the fakes 
   (probably with 'fake') have been satisfied, and check that the actual
   results are as expected. If the expected results are a function, it
   will be called with the actual result as its single argument.

   To strip tests from production code, set either clojure.test/*load-tests*
   or midje.semi-sweet/*check* to false."
  [& args]
  (error-let [[call-form arrow expected-result & other-stuff]
              (validate &form)]
    (when (user-desires-checking?)
      (let [ [fakes overrides] (fakes-and-overrides other-stuff)]
        (error-let [_ (spread-error (map validate fakes))]
          (expect-expansion call-form arrow expected-result fakes overrides))))))


