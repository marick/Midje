(ns ^{:doc "Mostly functions for identifying semi-sweet expects, and for converting 
            midje.sweet arrow forms into semi-sweet expect forms."}
  midje.parsing.1-to-explicit-form.expects
  (:use midje.parsing.util.core
        [midje.parsing.2-to-lexical-maps.expects :only [expect]])
  (:require [clojure.zip :as zip]
            [midje.parsing.util.zip :as pzip]
            [midje.parsing.util.overrides :as override]
            [midje.parsing.util.recognizing :as recognize]
            [midje.parsing.util.file-position :as position]))
  


;; Moving around

(defn up-to-full-expect-form
  "From anywhere (recursively) within an expect form, move so that
   loc is at the full form (so that zip/down is 'expect)." 
  [loc]
  (if (recognize/expect? loc)
    loc
    (recur (zip/up loc))))



(defn tack-on__then__at-same-location [[form & more-forms] loc]
  (assert (recognize/expect? loc))
  (if form
    (recur more-forms (zip/append-child loc form))	  
    (up-to-full-expect-form loc)))

(defn tack-on__then__at-rightmost-expect-leaf [forms loc]
  (let [tack (fn [loc] (tack-on__then__at-same-location forms loc))]
    (-> loc tack zip/down pzip/skip-to-rightmost-leaf)))

(defn wrap-with-expect__then__at-rightmost-expect-leaf [loc]
  (assert (recognize/start-of-checking-arrow-sequence? loc))
  (let [right-hand (-> loc zip/right zip/right)
        arrow-sequence (-> loc zip/right zip/node)
        additions (override/arrow-sequence-overrides (zip/rights right-hand))
        line-number (position/arrow-line-number (zip/right loc))
        edited-loc (zip/edit loc
                      (fn [loc]
                        (vary-meta
                          `(expect ~loc ~arrow-sequence ~(zip/node right-hand) ~@additions)
                          assoc :line line-number)))]
    (->> edited-loc
      zip/right
      (pzip/n-times (inc (count additions)) pzip/remove-moving-right)
      zip/remove)))
