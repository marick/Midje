(ns ^{:doc "Functions for turning provideds into explicit fakes"}
  midje.parsing.1-to-explicit-form.prerequisites
  (:require [clojure.zip :as zip]
            [midje.parsing.1-to-explicit-form.expects :as parse-expects]
            [midje.parsing.2-to-lexical-maps.data-fakes :as data-fakes]
            [midje.parsing.2-to-lexical-maps.fakes :as fakes]
            [midje.parsing.arrow-symbols :refer :all]
            [midje.parsing.util.core :refer :all]
            [midje.parsing.util.error-handling :as error]
            [midje.parsing.util.overrides :as override]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.zip :as pzip]
            [midje.util.ecosystem :as ecosystem]
            [pointer.core :as pointer]))

(defn prerequisite-to-fake [fake-body]
  (let [^Integer line-number (-> fake-body
                                 zip/seq-zip
                                 zip/down
                                 zip/right
                                 pointer/line-number-for)
        fake-tag (if (recognize/metaconstant-prerequisite? fake-body)
                   `data-fakes/data-fake
                   `fakes/fake)]
    (vary-meta
     `(~fake-tag ~@fake-body)
     assoc :line (Integer. line-number))))

(defn take-arrow-sequence
  "Extract the next arrow sequence from a longer sequence of forms."
  [forms]
  (let [constant-part (take 3 forms)
        overrides (override/arrow-sequence-overrides (nthnext forms 3))]
    (concat constant-part overrides)))

(defn pull-all-arrow-seqs-from
  ([fakes]
     (pull-all-arrow-seqs-from [] fakes))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [arrow-seq (take-arrow-sequence remainder)]
        (recur (conj so-far arrow-seq)
               (nthnext remainder (count arrow-seq)))))))

(defn- expand-prerequisites-into-fake-calls [provided-loc]
  (let [fakes (-> provided-loc zip/up zip/node rest)
        fake-bodies (pull-all-arrow-seqs-from fakes)]
    (map prerequisite-to-fake fake-bodies)))

(defn delete_prerequisite_form__then__at-previous-full-expect-form [loc]
  (assert (recognize/provided? loc))
  (-> loc zip/up zip/remove parse-expects/up-to-full-expect-form))


(defn insert-prerequisites-into-expect-form-as-fakes [loc]
  (if (recognize/immediately-following-check-form? loc)
    (let [fake-calls (expand-prerequisites-into-fake-calls loc)
          full-expect-form (delete_prerequisite_form__then__at-previous-full-expect-form loc)]
      (parse-expects/tack-on__then__at-rightmost-expect-leaf fake-calls full-expect-form))
    (error/report-error (zip/node (zip/up loc))
                        "The form before the `provided` is not a check:"
                        (pr-str (pzip/previous-form loc))
                        "Here are common errors when writing a form like the following:"
                        "   (f ..arg..) => 0"
                        "   (provided"
                        "     ...)"
                        "Misparenthesization: `(f ..arg.. => 0) (provided... `"
                        "Missing =>: `(f ..arg..) (provided...` ")))
