(ns ^{:doc "Metadata is attached to facts and fact tables"}
  midje.ideas.metadata
  (:use [midje.ideas.arrows :only [start-of-checking-arrow-sequence?]])
  (:require [clojure.string :as str]))

(def ^{:dynamic true} metadata-for-fact-group {})

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

(declare separate-metadata)
(defn fact-body-source [fact-function]
  (second (separate-metadata (fact-source fact-function))))

(defn separate-metadata [fact-form]
  (letfn [(basic-parse [metadata body]
            (let [head (first body)
                  add-key (fn [key value] (assoc metadata key value))]

              (cond (string? head)
                    (recur (add-key :midje/description head) (rest body))

                    (start-of-checking-arrow-sequence? body)
                    [metadata body]

                    (symbol? head)
                    (recur (add-key :midje/name (name head)) (rest body))

                    (keyword? head)
                    (recur (add-key head true) (rest body))

                    (map? head)
                    (recur (merge metadata head) (rest body))
                    
                    :else
                    [metadata body])))]
    (let [[metadata body] (basic-parse {:midje/source fact-form
                                        ;; Storing actual namespaces in these
                                        ;; maps causes bizarre errors in
                                        ;; seemingly unrelated code.
                                        :midje/namespace (ns-name *ns*)
                                        :midje/file *file*
                                        :midje/line (:line (meta fact-form))}
                                       (rest fact-form))
          metadata (if (and (contains? metadata :midje/description)
                            (not (contains? metadata :midje/name)))
                     (assoc metadata :midje/name (:midje/description metadata))
                     metadata)]
      [(merge metadata-for-fact-group metadata) body])))


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

(defmacro with-wrapped-metadata [metadata & body]
  `(binding [metadata-for-fact-group (merge metadata-for-fact-group ~metadata)]
         ~@body))

(defn without-automatic-metadata [metadata]
  (dissoc metadata :midje/source :midje/file :midje/line :midje/namespace))

(defn promote-metadata [outer-form]
  (let [[outer-metadata [inner-form & rest-of-outer-body]] (separate-metadata outer-form)
        [inner-metadata inner-body] (separate-metadata inner-form)]
    (cond (and (empty? (without-automatic-metadata outer-metadata))
               (not (empty? (without-automatic-metadata inner-metadata))))
          `(~(first outer-form)
            ~(merge outer-metadata (without-automatic-metadata inner-metadata))
            (~(first inner-form) ~@inner-body)
            ~@rest-of-outer-body)

          :else
          outer-form)))

