(ns midje.sweet.chained-fakes
  (:use midje.semi-sweet)
  (:use midje.sweet.util)
  (:require [clojure.zip :as zip])
  )

(only-mocked replace-with-two-links)

(defn form-metaconstant [inner-form]
  (symbol (format "...%s-link..." (first inner-form)))
  )

(defn form-to-pull-out [fake-form]
  (-> fake-form second second))

(defn replace-interior-function-with-metaconstant [fake-form interior-form placeholder]
  (let [one-level-replacement (fn [form replacement]
				(cons (first form) (cons replacement (rest (rest form)))))
	original-interior (second fake-form)
	new-interior (one-level-replacement original-interior placeholder)]
    (one-level-replacement fake-form new-interior)))


(defn unchain [ [fake call-part => result-part & keypairs :as fake-form] ]
  (let [interior-form (form-to-pull-out fake-form)
	placeholder (form-metaconstant interior-form)]
    [ `(fake ~interior-form => ~placeholder ~@keypairs)
      (replace-interior-function-with-metaconstant fake-form interior-form placeholder) ]))


(defn- true-function-symbol? [loc]
  (let [symbol (zip/node loc)]
    (not (= 'quote symbol))))

(defn looks-like-a-function-call? [loc first-symbol-validity-test]
;  (println "looks-like-a-function-call?" loc)
  (when-let [tree (and loc (zip/node loc))]
;    (println "does this look like a function call? " tree)
;    (println (type tree))
;    (println (list? tree))
    (and tree
	 (or (list? tree) (= (type tree) clojure.lang.Cons))
	 (-> tree first) 
	 (-> tree first symbol?) 
	 (-> loc zip/down first-symbol-validity-test))))

(defn- nested-function-like-list
  ([loc]
     (nested-function-like-list loc (fn [symbol] true)))
  ([loc first-symbol-validity-test]
;     (println "Processing: " (and loc (zip/node loc)))
     (when (looks-like-a-function-call? loc first-symbol-validity-test)
       (-> loc zip/down zip/right))))

(defn at-chained-fake? [loc]
  (-> loc
      ;; (fake ...) or something else
      (nested-function-like-list (fn [loc] (namespacey-match '(fake) loc)))
      ;; (f ...) or nil
      nested-function-like-list
      ;; (g ...) or nil
      (looks-like-a-function-call? true-function-symbol?)))

(defn replace-with-two-links__stay_put [fake-loc]
;  (println "replacing" (zip/node fake-loc))
  (let [replacements (unchain (zip/node fake-loc))
	replace-with-first (fn [loc] (zip/replace loc (first replacements)))
	append-second (fn [loc] (zip/insert-right loc (second replacements)))]
;    (println "with" (zip/node replacements))
    (-> fake-loc replace-with-first append-second)))

(defn rewrite [form]
  (loop [loc (zip/seq-zip form)]
    (if (zip/end? loc)
      (zip/root loc)
      (recur (zip/next (cond (not (zip/branch? loc))
			     loc
			     
			     (at-chained-fake? loc)
			     (replace-with-two-links__stay_put loc)
			     
			     :else loc))))))
	
