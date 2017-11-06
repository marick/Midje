(ns ^{:doc "A notation that avoids confusion between what’s essential
            about data and what’s accidental. A stand in for constant data."}
  midje.data.metaconstant
  (:require [midje.util.ecosystem :as ecosystem]
            [midje.util.exceptions :as exceptions]
            [midje.util.thread-safe-var-nesting :refer [unbound-marker]]))


;;; Metaconstants are built from special symbols, called "metaconstant symbol". Such symbols
;;; begin and end with either a dash or a period.

(def ^{:private true} formats {"..." #"^(\.+)([^.]+)(\.+)$"
                               "---" #"^(-+)(.+?)(-+)$"})

(defn- evaluate-comparison-potential [x]
  (letfn [(variant-and-body [variant]
            (if-let [[_ _ body _] (re-matches (formats variant) (name x))]
              [variant body]))]
    (and (instance? clojure.lang.Named x)
         (or (variant-and-body "...")
             (variant-and-body "---")))))

(defn metaconstant-symbol? [x]
  (boolean (and (symbol? x) (evaluate-comparison-potential x))))

(defn- name-for-comparison
  [x]
  (if-let [[variant body] (evaluate-comparison-potential x)]
    (str variant body variant)))

(defn- report-error [top-line]
  (exceptions/user-error
   top-line
   "If you have a compelling case for equality, please create an issue:"
   ecosystem/issues-url))

;;; Metaconstant proper

(deftype Metaconstant [underlying-symbol ^clojure.lang.Associative storage meta-data]
  Object
  (toString [this]
    (str (.underlying-symbol this)))
  (equals [^Metaconstant this that]
    (= (name-for-comparison this) (name-for-comparison that)))

  clojure.lang.Named
  (getNamespace [this]
    (.getNamespace ^clojure.lang.Symbol (.underlying-symbol this)))
  (getName [this]
    (name (.underlying-symbol this)))

  clojure.lang.ILookup
  (valAt [this key]
         (get storage key))
  (valAt [this key default]
         (get storage key default))

  clojure.lang.IFn
  (invoke [this key]
          (get storage key))
  (invoke [this key default]
          (get storage key default))

  clojure.lang.Associative
  (containsKey [this key]
               (.containsKey storage key))
  (entryAt [this key]
           (find storage key))
  (assoc [this key val]
    ;; (Metaconstant. (.underlying-symbol this) (assoc storage key val)))
    (throw (report-error (str "Metaconstants (" (.underlying-symbol this) ") can't have values assoc'd onto them."))))

  ;; Next two interfaces are extended by Associative.
  clojure.lang.Seqable
  (seq [this] (seq storage))  ; I'm unhappy to define this, but it's needed by count/empty.

  clojure.lang.Counted
  (count [this] (count storage))

  clojure.lang.IPersistentCollection
  (cons [this o]
        ;; (Metaconstant. (.underlying-symbol this) (cons storage o)))
        (throw (report-error (str "Metaconstants (" (.underlying-symbol this) ") can't have values added onto them."))))
  (empty [this]
         (empty storage))
  (equiv [this that]
         (if (or (symbol? that)
                 (= (type that) Metaconstant)
                 (= that unbound-marker))
           (.equals this that)
           (throw
             (report-error (str "Metaconstants (" (.underlying-symbol this) ") can't be compared for equality with "
                                (pr-str that) ".")))))

  ;; Interface that provide meta support.
  clojure.lang.IObj
  (meta [this] meta-data)
  (withMeta [this m] (Metaconstant. underlying-symbol storage m)))

(defn metaconstant [underlying-symbol ^clojure.lang.Associative storage meta-data]
  (when (not (map? storage))
    (throw (report-error (str "Metaconstants (" underlying-symbol ") can't represent non-map values " (pr-str storage) "."))))
  (Metaconstant. underlying-symbol storage meta-data))

(defn merge-metaconstants [^Metaconstant mc1 ^Metaconstant mc2]
  (Metaconstant. (.underlying-symbol mc1)
                 (merge (.storage mc1) (.storage mc2))
                 (merge (.meta-data mc1) (.meta-data mc2))))

(defmethod print-method Metaconstant [^Metaconstant o ^java.io.Writer w]
  (print-method (.underlying-symbol o) w))

(defmethod clojure.pprint/simple-dispatch Metaconstant [^Metaconstant m]
  (print m))
