(ns ^{:doc "Maintain the use of namespace-specific prerequisites"}
  midje.data.prerequisite-state
  (:require [such.control-flow :refer [branch-on]]
            [such.types :as types]
            [such.symbols :refer [+symbol]]
            [such.shorthand :refer [find-first]]
            [midje.checking.core :refer :all]
            [midje.config :as config]
            [midje.data.metaconstant :as mc]
            [midje.emission.api :as emit]
            [midje.emission.deprecation :refer [deprecate]]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.util.exceptions :as exceptions]
            [midje.util.pile :as pile]
            [midje.util.thread-safe-var-nesting :refer [with-altered-roots]]
            [such.maps :as map]
            [such.sequences :as seq]))


;;; Questions to ask of fakes // accessors

(defn implements-a-fake? [function]
  (:midje/faked-function (meta function)))

;;; Creating fake maps



;;; Handling mocked calls

(defmulti ^{:private true} call-handled-by-fake? (fn [function-var actual-args fake]
                                                   (:type fake)))

(defmethod call-handled-by-fake? :not-called [function-var actual-args fake]
  (= function-var (:var fake)))

(defmethod call-handled-by-fake? :default [function-var actual-args fake]
  (and (= function-var (:var fake))
       ((:arglist-matcher fake) actual-args)))

(defn usable-default-function? [fake]
  (and (config/choice :partial-prerequisites)
       (bound? (:var fake))
       (let [value-in-var (var-get (:var fake))
             unfinished-fun (:midje/unfinished-fun (meta (:var fake)))]
         (and (types/extended-fn? value-in-var)
              (or (nil? unfinished-fun)
                  (not= unfinished-fun value-in-var))))))

;; Used for IFn interface
(def #^:private ^{:testable true}
     default-function :value-at-time-of-faking)

(def #^:dynamic #^:private *call-action-count* (atom {}))
(defn nested-prerequisite-call? []
  (= 2 (get (deref *call-action-count*) (Thread/currentThread))))

(defn record-start-of-prerequisite-call []
  (swap! *call-action-count* update-in [(Thread/currentThread)] (fnil inc 0)))
(defn record-end-of-prerequisite-call []
  (swap! *call-action-count* update-in [(Thread/currentThread)] dec))

(defn- ^{:testable true} best-call-action
  "Returns a fake: when one can handle the call
   Else returns a function: from the first fake with a usable-default-function.
   Returns nil otherwise."
  [function-var actual-args fakes]
  (when (nested-prerequisite-call?)
    (throw (apply exceptions/user-error (parse-fakes/disallowed-function-failure-lines function-var))))
  (if-let [found (find-first (partial call-handled-by-fake? function-var actual-args) fakes)]
    found
    (when-let [fake-with-usable-default (find-first #(and (= function-var (:var %))
                                                          (usable-default-function? %))
                                                    fakes)]
      (default-function fake-with-usable-default))))

(defmacro ^:private counting-nested-calls [& forms]
  `(try
     (record-start-of-prerequisite-call)
     ~@forms
     (finally (record-end-of-prerequisite-call))))

(defn- valid-arg-counts
  "Get required and optional argument counts for a function variable"
  [function-var]
  (let [arglists                    (:arglists (meta function-var))
        [opt-arglists req-arglists] (seq/bifurcate #((set %) '&) arglists)]
    [(set (map count req-arglists))
     (and (seq opt-arglists) (apply min (map #(-> % count (- 2)) opt-arglists)))]))

(defn- correct-arg-count-message
  "Readable message describing a function's valid required/optional arg counts"
  [function-var]
  (let [[req-arg-counts-set
         opt-arg-min]       (valid-arg-counts function-var)
        req-counts          (when (not (empty? req-arg-counts-set))
                              (clojure.string/join ", " (-> req-arg-counts-set seq sort)))]
    (cond
      (and req-counts opt-arg-min)
      (str req-counts ", or " opt-arg-min "+ arguments")

      (and (not req-counts) opt-arg-min)
      (str opt-arg-min "+ arguments")

      :else
      (str req-counts " arguments"))))

(defn- correct-arg-count?
  "Does the argument count provided fit one of the function's defined arities?"
  [function-var provided-arg-count]
  (if-let [arglists (:arglists (meta function-var))]
    (let [[req-arg-counts-set
           opt-arg-min]       (valid-arg-counts function-var)
          in-req-arglists?    (req-arg-counts-set provided-arg-count)
          in-opt-arglists?    (and opt-arg-min
                                   (>= provided-arg-count opt-arg-min))]
      (or in-req-arglists?
          in-opt-arglists?))
    true))

(defn- fail-fake-arg-mismatch [function-var actual-args fakes]
  (emit/fail {:type :prerequisite-arg-count-mismatches-implementation
              :var function-var
              :provided-arg-count (count actual-args)
              :expected-arg-count-msg (correct-arg-count-message function-var)
              :position (:position (first fakes))})
  (format "`%s` returned this string because it was faked with an
          incorrect number of arguments with respect to the function
          implementation"
          (+symbol function-var)))

(defn- fail-fake-unexpected-args [function-var actual-args fakes]
  (emit/fail {:type :prerequisite-was-called-with-unexpected-arguments
              :var function-var
              :actual actual-args
              :position (:position (first fakes))})
  (format "`%s` returned this string because it was called with an unexpected argument"
          (+symbol function-var)))

(defn- ^{:testable true} handle-mocked-call [function-var actual-args fakes]
  (let [action (counting-nested-calls (best-call-action function-var actual-args fakes))]
    (branch-on action
      ;; default fall-through for when :partial-prerequisites config is set
      types/extended-fn?
      (apply action actual-args)

      ;; standard case
      map?
      (if (correct-arg-count? function-var (count actual-args))
        (do
          (swap! (:call-count-atom action) inc)
          ((:result-supplier action)))
        (fail-fake-arg-mismatch function-var actual-args fakes))

      :else
      (fail-fake-unexpected-args function-var actual-args fakes))))

;; Binding map related

(defn- fn-fakes-binding-map [fn-fakes]
  (let [var->faker-fn (fn [the-var]
                        (-> (fn [& actual-args]
                              (handle-mocked-call the-var actual-args fn-fakes))
                            (vary-meta assoc :midje/faked-function true)))
        fn-fake-vars (map :var fn-fakes)]
    (zipmap fn-fake-vars
            (map var->faker-fn fn-fake-vars))))

(defn- data-fakes-binding-map [data-fakes]
  (let [metaconstant-vars (for [{:keys [var contained]} data-fakes]
                            {var (mc/metaconstant (pile/object-name var) contained nil)})]
    (apply merge-with mc/merge-metaconstants metaconstant-vars)))

(defn binding-map [fakes]
  (let [[data-fakes fn-fakes] (seq/bifurcate :data-fake fakes)]
    (merge (fn-fakes-binding-map fn-fakes)
           (data-fakes-binding-map data-fakes))))

(defmacro with-installed-fakes [fakes & forms]
  `(with-altered-roots (binding-map ~fakes)
     ~@forms))
