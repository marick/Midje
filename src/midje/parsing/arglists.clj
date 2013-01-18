(ns ^{:doc "Parsing function argument lists"}
  midje.parsing.arglists
  (:use [midje.error-handling.exceptions :only [user-error]]
        clojure.pprint)
  (:require [midje.emission.levels :as levels]
            [midje.config :as config]
            [midje.util.form-utils :as form]))

(defn separate-print-levels [args default]
  (let [[[print-level & extras] non-levels] (form/separate-by levels/valids args)]
    (when (seq extras)
      (throw (user-error "You have extra print level names or numbers.")))
    (dorun (map levels/validate-level! (filter number? args)))
      
    (if print-level
      [[print-level]  print-level   non-levels]
      [[           ]  default       non-levels])))


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

               
          
          
