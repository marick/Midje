(ns ^{:doc "Facts as a data structure"}
  midje.data.fact
  (:refer-clojure :exclude [name namespace]))


;;; This is a potential target for migrating the non-parsing,
;;; non-checking parts of ideas/facts.clj, ideas/metadata.clj. It's
;;; sketchy now, since it's only being used to keep the emission
;;; plugin code from depending on code slated to be destroyed.

(def fact-properties
  [:midje/source :midje/guid :midje/file :midje/line :midje/namespace
   :midje/name :midje/description :midje/top-level?])

(defn make-getters
  "Create midje.data.fact/name midje.data.fact/file, etc."
  [in-namespace prefix]
  (doseq [property-key fact-properties]
    (let [property-string (clojure.core/name property-key)
          function-symbol (symbol (str prefix property-string))

          annotated-function-symbol
          (vary-meta function-symbol assoc
                     :arglists '([fact])
                     :doc (str "Return the " property-string
                               " of the given fact.\n  Facts are in the form returned by `fetch-facts`."))]
      (intern *ns*
              annotated-function-symbol
              (comp property-key meta)))))
(make-getters *ns* "")

(defn best-description [fact]
  (or (description fact)
      (name fact)
      nil))


(def allows-itself-to-be-recorded? (comp not :check-only-at-load-time meta))
