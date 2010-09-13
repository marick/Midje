(ns midje.fact-body-transformation
  (:use clojure.test
	midje.semi-sweet
        [clojure.contrib.ns-utils :only [immigrate]])
  (:require [clojure.zip :as zip])
)

;; Identifying forms

;; TODO: There must be a better way of handling namespaces.
(defn namespacey-match [symbols loc]
  (let [base-names (map name symbols)
	qualified-names (map #(str "midje.semi-sweet/" %) base-names)]
    ( (set (concat base-names qualified-names)) (str (zip/node loc)))))

(defn is-semi-sweet-keyword? [loc]
  (namespacey-match '(expect fake) loc))

(defn head-of-provided-form? [loc]
  (namespacey-match '(provided) loc))

(defn at-full-expect-form? [loc]
  (and (zip/branch? loc)
       (namespacey-match '(expect) (zip/down loc))))

(defn start-of-arrow-sequence? [loc]
  (and (zip/right loc)
       (namespacey-match '(=>) (zip/right loc))))


;; Yielding ordinary forms

(defn overrides [possibles]
  (apply concat (take-while (comp keyword? first) (partition 2 possibles))))

(defn partition-fake-bodies
  ([expectations]
     (partition-fake-bodies [] expectations))
  ([so-far remainder]
    (if (empty? remainder)
      so-far
      (let [constant-part (take 3 remainder)
	    overrides (overrides (nthnext remainder 3))
	    whole-body (concat constant-part overrides)]
	(recur (conj so-far whole-body)
	       (nthnext remainder (count whole-body)))))))

;; Yeah, it's not tail-recursive. So sue me.
(defn arrow-line-number [arrow-loc]
  (try (or  (-> arrow-loc zip/left zip/node meta :line)
	    (-> arrow-loc zip/right zip/node meta :line)
	    (inc (arrow-line-number (zip/prev arrow-loc))))
       (catch Throwable ex nil)))

;; Simple movement

(defn up-to-full-expect-form [loc]
  (if (at-full-expect-form? loc)
    loc
    (recur (zip/up loc))))

(defn skip-to-end-of-full-form [loc]
  (let [end-form (zip/rightmost loc)]
    (if (zip/branch? end-form)
      (recur (zip/down end-form))
      end-form)))

(defn n-times [n zip-fn loc]
  (if (zero? n)
    loc
    (recur (dec n) zip-fn (zip-fn loc))))

;; Editing

(defn remove-moving-right [loc]
  (-> loc zip/remove zip/next)
)

(defn delete-enclosing-provided-form__at-previous-full-expect-form [loc]
  (assert (head-of-provided-form? loc))
  (let [x (-> loc zip/up zip/remove)]
    (up-to-full-expect-form x)))

(defn tack-on__at-same-location [forms loc]
  (assert (at-full-expect-form? loc))
  (if (empty? forms)
    (up-to-full-expect-form loc)
    (recur (rest forms) (zip/append-child loc (first forms)))))

(defn tack-on__at-rightmost-expect-location [forms loc]
  (let [tack (fn [loc] (tack-on__at-same-location forms loc))]
    (-> loc tack zip/down skip-to-end-of-full-form)))

(defn wrap-with-expect__at-rightmost-wrapped-location [loc]
  (assert (start-of-arrow-sequence? loc))
  (let [right-hand (-> loc zip/right zip/right)
	additions (overrides (zip/rights right-hand))
	edited-loc (zip/edit loc
			     (fn [loc] `(expect ~loc => ~(zip/node right-hand) ~@additions)))]
    (->> edited-loc
	 zip/right
	 (n-times (+ 1 (count additions)) remove-moving-right)
	 zip/remove)))


;; 

;; The meat of it.
(defn expand-following-into-fake-calls [provided-loc]
  (let [expectations (rest (zip/node (zip/up provided-loc)))
	fake-bodies (partition-fake-bodies expectations)]
    (map (fn [fake-body] `(midje.semi-sweet/fake ~@fake-body))
	 fake-bodies)))

(defn rewrite [multi-form]
  (loop [loc (zip/seq-zip multi-form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (start-of-arrow-sequence? loc)
			     (wrap-with-expect__at-rightmost-wrapped-location loc)

			     (head-of-provided-form? loc)
			     (let [fake-calls (expand-following-into-fake-calls loc)
				   full-expect-form (delete-enclosing-provided-form__at-previous-full-expect-form loc)]
			       (tack-on__at-rightmost-expect-location fake-calls full-expect-form))

			     (is-semi-sweet-keyword? loc)
			     (throw (Exception. (str "Why is this form processed twice?" (zip/node loc))))
			     ; See comment in ignores-semi-sweet-constructs-test
;			     (skip-to-end-of-full-form loc)

			     :else loc))))))
	
