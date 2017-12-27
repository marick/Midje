(ns ^{:doc "Parsing metadata as found in facts, around-facts, and tables"}
  midje.parsing.1-to-explicit-form.metadata
  (:require [midje.parsing.util.recognizing :as recognize]
            [midje.util.exceptions :refer [user-error]]
            [such.random :as random]))

(def ^{:dynamic true} metadata-for-fact-group {})

(defmacro with-wrapped-metadata [metadata & body]
  `(binding [metadata-for-fact-group (merge metadata-for-fact-group ~metadata)]
         ~@body))

(defn separate-metadata
  "Removes metadata from a form and returns it along with the metadate-free
   remainder. Also assigns the Midje core metadata like :midje/source.
   However, it does not override such key/value pairs when explicitly supplied."
  [metadata-containing-form]
  (letfn [(basic-parse [metadata body]
            (let [head (first body)
                  add-key (fn [key value] (assoc metadata key value))]

              (cond (recognize/start-of-checking-arrow-sequence? body)
                    [metadata body]

                    (string? head)
                    (recur (add-key :midje/description head) (rest body))

                    (symbol? head)
                    (recur (add-key :midje/name (name head)) (rest body))

                    (keyword? head)
                    (recur (add-key head true) (rest body))

                    (map? head)
                    (recur (merge metadata head) (rest body))

                    :else
                    [metadata body])))]
    (let [[metadata body] (basic-parse {:midje/source `'~metadata-containing-form
                                        ;; Storing actual namespaces in these
                                        ;; maps causes bizarre errors in
                                        ;; seemingly unrelated code.
                                        :midje/namespace `'~(ns-name *ns*)
                                        :midje/file *file*
                                        :midje/line (:line (meta metadata-containing-form))}
                                       (rest metadata-containing-form))
          ;; names and descriptions influence each other
          metadata (if (and (contains? metadata :midje/description)
                            (not (contains? metadata :midje/name)))
                     (assoc metadata :midje/name (:midje/description metadata))
                     metadata)
          ;; Add guid unless it was passed in.
          metadata (merge {:midje/guid (random/form-hash body)} metadata)]
      [(merge metadata-for-fact-group metadata) body])))

(defn unparse-metadata
  "Returns a sequence containing a name (if any), a description (if any),
   and a map containing key/value pairs for user supplied metadata.
   Midje core metadata (like :midje/line) is not included."
  [metadata]
  (let [name (:midje/name metadata)
        description (:midje/description metadata)
        namelike (cond (and name (not description))
                       [(symbol name)]

                       (and description (not name))
                       (throw (Error. "This case is impossible"))

                       (and (not description) (not name))
                       []

                       (= description name)
                       [description]

                       :else
                       [(symbol name) description])
        maplike (apply dissoc metadata (filter #(re-find #"^:midje/" (str %)) (keys metadata)))]
    (if (empty? maplike)
      namelike
      (cons maplike namelike))))

(defn separate-two-level-metadata [top-form]
  (let [[top-level-meta top-level-body] (separate-metadata top-form)
        lower-level-form (first top-level-body)
        [lower-level-meta lower-level-body] (separate-metadata lower-level-form)
        stripped-top-level-body `((~(first lower-level-form) ~@lower-level-body) ~@(rest top-level-body))]
      [(merge lower-level-meta top-level-meta {:midje/guid (random/form-hash stripped-top-level-body)})
       stripped-top-level-body]))


(defn separate-multi-fact-metadata
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
