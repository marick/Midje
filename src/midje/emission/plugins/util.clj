(ns ^{:doc "General purpose plugin utilities"}
  midje.emission.plugins.util
  (:require [clojure.repl :refer [demunge]]
            [clojure.string :as str]
            [puget.printer :as puget]
            [midje.util.pile :as pile]
            [midje.emission.clojure-test-facade :as ctf]
            [midje.emission.colorize :as color]
            [midje.util.exceptions :as exception]
            [midje.config :as config]
            [midje.util.ordered-map :as om]
            [midje.data.metaconstant :as mc]
            [midje.util.ordered-set :as os])
  (:import [midje.data.metaconstant Metaconstant]))

;; The theory here was that using clojure.test output would allow text
;; from failing *facts* to appear within the clojure.test segment of
;; summary output. That doesn't work (though it works fine for
;; clojure.test output). The whole rigamarole is boring and I don't
;; care to jump through any more hoops. I say it's spinach, and I say
;; to hell with it.

;; (Don't change this to println, though. That doesn't work either.
;; Newlines are lost.)
(def emit-one-line ctf/output)
(defn emit-lines [lines]
  (doseq [line lines] (emit-one-line line)))

(defn indented [lines]
  (map (partial str "        ") lines))

;;; The following is taken from Alex Baranosky's gui-diff
;;; https://github.com/AlexBaranosky/gui-diff
;;; Extracted because of its dependencies

(defn- map-keys [f m]
  (zipmap
   (map f (keys m))
   (vals m)))

(defn- last-piece-of-ns-qualified-class-name [clazz]
  (last (clojure.string/split (str clazz) #"\.")))

(defn- grouped-comparables-and-uncomparables [xs]
  (let [[comparable uncomparable] ((juxt filter remove)
                                   #(instance? java.lang.Comparable %) xs)
        group+format+sort (fn [xs]
                            (->> (group-by class xs)
                                 (map-keys last-piece-of-ns-qualified-class-name)
                                 (into (sorted-map))))]
    [(group+format+sort comparable)
     (group+format+sort uncomparable)]))

(defn- safe-sort
  "Attempts sorting input but defaults returning the list unsorted when
  elements are heterogeneous, and hence unsortable"
  [xs]
  (try (sort xs) (catch ClassCastException e xs)))

(defn nested-sort
  "Sorts two nested collections for easy visual comparison.
   Sets and maps are converted to order-sets and ordered-maps."
  [x]
  (letfn [(seq-in-order-by-class
            [class-name->items sort?]
            (for [[_clazz_ xs] class-name->items
                  x (if sort? (safe-sort xs) xs)]
              x))
          (map-in-order-by-class
            [m class-name->keys sort?]
            (into (om/ordered-map)
                  (for [[_clazz_ ks] class-name->keys
                        k (if sort? (safe-sort ks) ks)]
                    [k (nested-sort (get m k))])))]

    (cond (set? x)
          (let [[comps uncomps] (grouped-comparables-and-uncomparables x)]
            (into (os/ordered-set)
                  (concat (seq-in-order-by-class comps true)
                          (seq-in-order-by-class uncomps false))))

          (map? x)
          (let [[comps uncomps] (grouped-comparables-and-uncomparables (keys x))]
            (into (map-in-order-by-class x comps true)
                  (map-in-order-by-class x uncomps false)))

          (vector? x)
          (into [] (map nested-sort x))

          (list? x)
          (reverse (into '() (map nested-sort x)))

          :else
          x)))

(def sorted-if-appropriate nested-sort) ; backward compatibility

(defn linearize-lines
  "Takes a nested structure that contains nils and strings.
   Converts it into a simple sequence of strings."
  [messy-lines]
  (->> messy-lines flatten (remove nil?)))

(defn function-name
  "Convert a function into a readable name, if possible."
  [function]
  (->> (.getName (class function))
       demunge
       ( #(str/split % #"\.|/"))
       (take-last 2)
       (str/join "/")))

(defn prerequisite-var-description
  "Takes a var naming a prerequisite and returns a string useful for printing"
  [prerequisite-var]
  ;; This emphasizes a little that the prerequisite is a var, without having
  ;; too much spewage. While it's nice to be able to write:
  ;;    (provided (foo 3) => 4)
  ;; ... that can cause confusion in those (relatively uncommon) cases where
  ;; rebinding `#'foo` does not change the value of `foo`. Having been reminded
  ;; occasionally that she's working with vars might help a perplexed user
  ;; become unperplexed.
  (let [name (pile/object-name prerequisite-var)]
    (if name
      (str "#'" name)
      (pr-str prerequisite-var))))

(defn attractively-stringified-value
  "Does some standard prettification of forms:
        : a function named `foo`
        : a nicely printed stack trace
        : maps and sets sorted by key."
  [v]
  (if (exception/captured-throwable? v)
    (exception/friendly-stacktrace v)
    (let [raw-str (cond (fn? v)     (function-name v)
                        (record? v) v
                        :else       (nested-sort v))]
      (cond
        (config/choice :pretty-print)
        (puget/cprint-str raw-str {:print-handlers {Metaconstant puget/pr-handler}
                                   :print-fallback :pretty
                                   :seq-limit      10
                                   :map-delimiter  ""})
        (string? raw-str)
        raw-str

        :else
        (pr-str raw-str)))))

(defn format-nested-descriptions
  "Takes vector like [\"about cars\" nil \"sports cars are fast\"] and returns non-nils joined with -'s
   => \"about cars - sports cars are fast\""
  [nested-description-vector]
  (when-let [non-nil (seq (remove nil? nested-description-vector))]
    (str/join " - " non-nil)))

(defn filename-lineno
  "The ordinary way to describe the location of a failure."
  [[filename line-num]]
  (format "(%s:%s)" filename line-num))

(defn position-str
  "Describe a failure with optional namespace"
  [[filename line-num] namespace]
  (let [namespace (if namespace (str "  " namespace) "")]
    (if (config/choice :visible-failure-namespace)
      (format "(%s:%s%s)" filename line-num namespace)
      (filename-lineno [filename line-num]))))


(defn- format-binding-map [binding-map]
  (let [formatted-entries (for [[k v] binding-map]
                            (str (pr-str k) " " (pr-str v)))]
    (str "[" (str/join "\n                           " formatted-entries) "]")))



(defn failure-notice
  "The reader's eye is guided by a bright red FAIL, the filename and lineno, and
   perhaps this other information:
     : the descriptions of all enclosing facts, if any
     : notes on which bindings were supplied to a tabular fact"
  [m]
  (let [description (when-let [doc (format-nested-descriptions (:description m))]
                      (str (pr-str doc) " "))
        position (position-str (:position m) (:namespace m))
        table-substitutions (when (:midje/table-bindings m)
                              (str "With table substitutions: " (format-binding-map (:midje/table-bindings m))))]
    (list
     (str "\n" (color/fail "FAIL") " " description "at " position)
     table-substitutions)))
