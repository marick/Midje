(ns ^{:doc "Parsing function argument lists"}
  midje.parsing.other.arglists
  (:use midje.clojure.core
        midje.parsing.util.core
        [midje.error-handling.exceptions :only [user-error]])
  (:require [midje.emission.levels :as levels]
            [midje.config :as config]
            [midje.util.form-utils :as form]
            [midje.data.fact :as fact]))


;;;                                           Print levels (keywords)

(defn separate-print-levels [args default]
  (let [[[print-level & extras] non-levels] (separate levels/valids args)]
    (when (seq extras)
      (throw (user-error "You have extra print level names or numbers.")))
    (dorun (map levels/validate-level! (filter number? args)))
      
    (if print-level
      [[print-level]  print-level   non-levels]
      [[           ]  default       non-levels])))


;;;                                           Metadata filters


(def describes-name-matcher? stringlike?)
(defn describes-callable-matcher? [arg]
  (or (fn? arg) (keyword? arg)))

(defn name-matcher-for [desired]
  #(stringlike-matches? desired (fact/name %)))
(defn callable-matcher-for [desired]
  (comp desired meta))

(defn appropriate-matcher-for [desired]
  ( (if (describes-name-matcher? desired) name-matcher-for callable-matcher-for) 
    desired))

(defn desired-fact-predicate-from [desireds]
  (vary-meta
     (form/any-pred-from (map appropriate-matcher-for desireds))
     assoc :created-from desireds))


(defn separate-filters
  ([args plain-argument?]
     (let [[filters remainder]
           (separate #(and (not (plain-argument? %))
                           ((form/any-pred-from [string? regex? fn? keyword?]) %))
                     args)]
       [filters
        (desired-fact-predicate-from filters)
        remainder]))
  ([args]
     (separate-filters args (constantly false))))





;;;                                           Keyword options with 0 or more arguments.

(defn is-flag-segment-for? [flag-predicate]
  (comp boolean flag-predicate first))

(defn build-on-flag-keyword [original suffix]
  (keyword (str (name original) suffix)))

;; TODO: Be responsible, Marick, and break this into pieces.
;; Also, lein-midje should use this.
(defn make-option-arglist-parser [& flag-descriptions]
  (fn [arglist]
    (let [all-arg-set (set (flatten flag-descriptions))
          segments (partition-by all-arg-set arglist)
          [[true-args] flag-segments] (split-with (complement (is-flag-segment-for? all-arg-set))
                                                  segments)
          [unmentioned-keys map-with-flag-data-added]
          (loop [so-far {:true-args (vec true-args)}
                 flag-selector (zipmap (map set flag-descriptions)
                                       (map first flag-descriptions))
                                       
                 flag-segments flag-segments]
            (let [[[flag] flag-args] (take 2 flag-segments)
                  matching-selector (first (filter #(% flag) (keys flag-selector)))]
              (cond (empty? flag-segments)
                    [(vals flag-selector) so-far]
                    
                    matching-selector
                    (recur (assoc so-far (build-on-flag-keyword (flag-selector matching-selector) "?") true
                                  (build-on-flag-keyword (flag-selector matching-selector) "-args") (vec flag-args))
                           (dissoc flag-selector matching-selector)
                           (drop 2 flag-segments))
                    
                    :else
                    (throw (Error. (str "It should be impossible for a flag-segment not to match a selector."
                                        (pr-str flag-descriptions arglist)))))))]
      (merge map-with-flag-data-added
             (zipmap (map #(build-on-flag-keyword % "?") unmentioned-keys)
                     (repeat false))))))

               
          
          
