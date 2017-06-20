(ns ^{:doc "Maintain the use of namespace-specific prerequisites"}
  midje.data.prerequisite-state
  (:require [clojure.tools.macro :as macro]
            [commons.clojure.core :refer :all :exclude [any?]]
            [midje.checking.core :refer :all]
            [midje.config :as config]
            [midje.data.metaconstant :as metaconstant]
            [midje.emission.api :as emit]
            [midje.emission.deprecation :refer [deprecate]]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes]
            [midje.util.exceptions :as exceptions]
            [midje.util.pile :as pile]
            [midje.util.thread-safe-var-nesting :refer [with-altered-roots]]
            [such.maps :as map]
            [such.sequences :as seq])
  (:import midje.data.metaconstant.Metaconstant))


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
         (and (extended-fn? value-in-var)
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



(defn- ^{:testable true } best-call-action
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

(defn- ^{:testable true } handle-mocked-call [function-var actual-args fakes]
  (macro/macrolet [(counting-nested-calls [& forms]
               `(try
                  (record-start-of-prerequisite-call)
                  ~@forms
                  (finally (record-end-of-prerequisite-call))))]

    (let [action (counting-nested-calls (best-call-action function-var actual-args fakes))]
      (branch-on action
        extended-fn?
        (apply action actual-args)

        map?
        (do
          (swap! (:call-count-atom action) inc)
          ((:result-supplier action )))

        :else
        (do
          (emit/fail {:type :prerequisite-was-called-with-unexpected-arguments
                      :var function-var
                      :actual actual-args
                      :position (:position (first fakes))})
          (format "`%s` returned this string because it was called with an unexpected argument"
                  (+symbol function-var)))))))


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
  (apply merge-with metaconstant/merge-metaconstants (for [{:keys [var contained]} data-fakes]
                                                       {var (Metaconstant. (pile/object-name var) contained nil)})))

(defn binding-map [fakes]
  (let [[data-fakes fn-fakes] (seq/bifurcate :data-fake fakes)]
    (merge (fn-fakes-binding-map fn-fakes)
           (data-fakes-binding-map data-fakes))))

(defmacro with-installed-fakes [fakes & forms]
  `(with-altered-roots (binding-map ~fakes)
     ~@forms))


