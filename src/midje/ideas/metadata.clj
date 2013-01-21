(ns ^{:doc "Metadata is attached to facts and fact tables"}
  midje.ideas.metadata
  (:use [midje.ideas.arrows :only [start-of-checking-arrow-sequence?]])
  (:require [midje.util.form-utils :as form])
  (:require [clojure.string :as str]
            [midje.parsing.metadata :as parse-metadata]))

;;; Loaded facts are stored as functions with this metadata:

(def fact-properties
  [:midje/source :midje/file :midje/line :midje/namespace
   :midje/name :midje/description])

(defn metadata-function-name [property]
  (symbol (str "fact-" (name property))))

(doall (map (fn [property]
              (intern *ns* (vary-meta
                            (metadata-function-name property) assoc
                            :arglists '([fact-function])
                            :doc (str "Return the " (name property) " of the given fact.\n  Facts are in the form returned by `fetch-facts`."))
                      (fn [fact-function]
                        (property (meta fact-function)))))
            fact-properties))

(defn wrappable-metadata
  "This does not include metadata specified by strings or symbols."
  [forms]
  (loop [metadata {}
         [x & xs :as body] forms]
    (cond (keyword? x)
          (recur (assoc metadata x true) xs)
          
          (map? x)
          (recur (merge metadata x) xs)
          
          :else
          [metadata body])))

(defn without-automatic-metadata [metadata]
  (dissoc metadata :midje/source :midje/file :midje/line :midje/namespace))

                                        ;;; Working with metadata

(def describes-name-matcher? form/stringlike?)
(defn describes-callable-matcher? [arg]
  (or (fn? arg) (keyword? arg)))

(defn name-matcher-for [desired]
  #(form/stringlike-matches? desired (fact-name %)))
(defn callable-matcher-for [desired]
  (comp desired meta))
(defn appropriate-matcher-for [desired]
  ( (if (describes-name-matcher? desired) name-matcher-for callable-matcher-for) 
    desired))

(defn desired-fact-predicate-from [desireds]
  (vary-meta
     (form/any-pred-from (map appropriate-matcher-for desireds))
     assoc :created-from desireds))

(defn separate-filters [args plain-argument?]
  (let [[filters remainder]
        (form/separate-by #(and (not (plain-argument? %))
                                ((form/any-pred-from [string? form/regex? fn? keyword?]) %))
                          args)]
    [filters
     (desired-fact-predicate-from filters)
     remainder]))

