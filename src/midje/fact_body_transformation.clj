(ns midje.fact-body-transformation
  (:use clojure.test
	midje.semi-sweet
        [clojure.contrib.ns-utils :only [immigrate]])
  (:require [clojure.zip :as zip])
)

(defn ignore-form-headed-by? [loc]
  (#{'expect 'fake} (zip/node loc)))

(defn fake-source? [loc]
  (= 'provided (zip/node loc)))

(defn expand-into-fake-calls [form-headed-by-keyword]
  (let [expectations (rest form-headed-by-keyword)
	triplets (vec (partition 3 expectations))]
    (map (fn [triplet] `(midje.semi-sweet/fake ~@triplet))
	 triplets)))

(defn skip [loc]
  (zip/rightmost loc))

(defn delete-fake-source [loc]
  (-> loc zip/up zip/remove zip/up))

(defn tack-on [form loc]
  (let [append-location (zip/append-child loc form)]
    (-> append-location zip/right zip/prev)))

(defn needs-wrapping-with-expect? [loc]
  (and (zip/right loc)
       (= (-> loc zip/right zip/node) '=>))
)

(defn wrap-with-expect [loc]
  (let [right-hand (-> loc zip/right zip/right)
	edited-loc (zip/edit loc
			     (fn [loc] `(expect ~loc => ~(zip/node right-hand))))]
    (-> edited-loc zip/right zip/right zip/remove zip/remove)))

(defn rewrite [multi-form]
  (loop [loc (zip/seq-zip multi-form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (needs-wrapping-with-expect? loc)
			     (wrap-with-expect loc)

			     (ignore-form-headed-by? loc)
			     (skip loc)

			     :else loc))))))
	
