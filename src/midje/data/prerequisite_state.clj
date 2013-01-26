(ns ^{:doc "The semi-sweet representation of provided forms."}
  midje.data.prerequisite-state
  (:use midje.clojure.core
        [midje.util.object-utils :only [object-name]]
        midje.util.form-utils
        [midje.checking.extended-equality :only [extended-= extended-list-=]]
        [midje.util.thread-safe-var-nesting :only [namespace-values-inside-out
                                                   with-altered-roots]]
        [midje.util.deprecation :only [deprecate]]
        [midje.parsing.arrow-symbols]
        [clojure.tools.macro :only [macrolet]])
  (:require [midje.data.metaconstant :as metaconstant]
            [midje.config :as config]
            [midje.emission.api :as emit]
            [midje.parsing.2-to-lexical-maps.fakes :as parse-fakes])
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
       (= (count actual-args) (count (:arg-matchers fake)))
       (extended-list-= actual-args (:arg-matchers fake))))


(defn usable-default-function? [fake]
  (when config/*allow-default-prerequisites*
    (deprecate "*allow-default-prerequisites* is deprecated and will be removed in Midje 1.6.\nUse config variable :allow-default-prerequisites instead."))
  (and (or config/*allow-default-prerequisites*
           (config/choice :partial-prerequisites))
       (bound? (:var fake))
       (let [value-in-var (var-get (:var fake))
             unfinished-fun (:midje/unfinished-fun (meta (:var fake)))]
         (and (extended-fn? value-in-var)
              (or (nil? unfinished-fun)
                  (not= unfinished-fun value-in-var))))))

;; Used for IFn interface
(def #^:private ^{:testable true}
     default-function :value-at-time-of-faking)

(def #^:dynamic #^:private *call-action-count* (atom 0))


(defn- ^{:testable true } best-call-action 
  "Returns a fake: when one can handle the call
   Else returns a function: from the first fake with a usable-default-function.
   Returns nil otherwise."
  [function-var actual-args fakes]
  (when (= 2 @*call-action-count*)
    (parse-fakes/raise-disallowed-prerequisite-error function-var))
  (if-let [found (find-first (partial call-handled-by-fake? function-var actual-args) fakes)]
    found
    (when-let [fake-with-usable-default (find-first #(and (= function-var (:var %)) 
                                                          (usable-default-function? %)) 
                                                    fakes)]
      (default-function fake-with-usable-default))))

(defn- ^{:testable true } handle-mocked-call [function-var actual-args fakes]
  (macrolet [(counting-nested-calls [& forms]
               `(try
                  (swap! *call-action-count* inc)
                  ~@forms
                  (finally (swap! *call-action-count* dec))))]

    (let [action (counting-nested-calls (best-call-action function-var actual-args fakes))]
      (pred-cond action
        extended-fn?  (apply action actual-args)
        map?          (do
                        (swap! (:call-count-atom action) inc)
                        ((:result-supplier action )))
        :else (emit/fail {:type :prerequisite-was-called-with-unexpected-arguments
                          :var function-var
                          :actual actual-args
                          :position (:position (first fakes))})))))


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
                                                       {var (Metaconstant. (object-name var) contained)})))

(defn binding-map [fakes]
  (let [[data-fakes fn-fakes] (separate :data-fake fakes)]
    (merge (fn-fakes-binding-map fn-fakes) 
           (data-fakes-binding-map data-fakes))))

(defmacro with-installed-fakes [fakes & forms]
  `(with-altered-roots (binding-map ~fakes)
     ~@forms))


