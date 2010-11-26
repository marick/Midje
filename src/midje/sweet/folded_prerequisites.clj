(ns midje.sweet.folded-prerequisites
  (:use midje.semi-sweet)
  (:use midje.util.recognizing-midje-forms)
  (:require [clojure.zip :as zip])
  )

(def *unfolded-prerequisite-counts*)

(defn form-metaconstant [inner-form]
  (let [name (first inner-form)
	swap-fn (fn [current-value name]
		  (if (current-value name)
		    (assoc current-value name (inc (current-value name)))
		    (assoc current-value name 1)))
	number ((swap! *unfolded-prerequisite-counts* swap-fn name) name)]
    (symbol (format "...%s-value-%s..." name number))))

(defn form-to-pull-out [fake-form]
  (-> fake-form second second))

(defn replace-interior-function-with-metaconstant [fake-form interior-form placeholder]
  (let [one-level-replacement (fn [form replacement]
				(cons (first form) (cons replacement (rest (rest form)))))
	original-interior (second fake-form)
	new-interior (one-level-replacement original-interior placeholder)]
    (one-level-replacement fake-form new-interior)))


(defn unfold [ [fake call-part => result-part & keypairs :as fake-form] ]
  (let [interior-form (form-to-pull-out fake-form)
	placeholder (form-metaconstant interior-form)]
    [ `(fake ~interior-form => ~placeholder ~@keypairs)
      (replace-interior-function-with-metaconstant fake-form interior-form placeholder) ]))


(defn- mockable-function-symbol? [loc]
  (let [symbol (zip/node loc)]
    (not (or (= 'quote symbol)
	     (:midje/checker (meta (resolve symbol)))))))

(defn looks-like-a-function-call? [loc first-symbol-validity-test]
  (when-let [tree (and loc (zip/node loc))]
    (and tree
	 (or (list? tree) (= (type tree) clojure.lang.Cons))
	 (-> tree first) 
	 (-> tree first symbol?) 
	 (-> loc zip/down first-symbol-validity-test))))

(defn- nested-function-like-list
  ([loc]
     (nested-function-like-list loc (fn [symbol] true)))
  ([loc first-symbol-validity-test]
     (when (looks-like-a-function-call? loc first-symbol-validity-test)
       (-> loc zip/down zip/right))))

(defn at-folded-prerequisite? [loc]
  (-> loc
      ;; (fake ...) or something else
      (nested-function-like-list (fn [loc] (namespacey-match '(fake) loc)))
      ;; (f ...) or nil
      nested-function-like-list
      ;; (g ...) or nil
      (looks-like-a-function-call? mockable-function-symbol?)))

(defn replace-with-two-prerequisites__stay_put [fake-loc]
  (let [replacements (unfold (zip/node fake-loc))
	replace-with-first (fn [loc] (zip/replace loc (first replacements)))
	append-second (fn [loc] (zip/insert-right loc (second replacements)))]
    (-> fake-loc replace-with-first append-second)))

(defmacro with-count-atom [& forms]
  `(binding [*unfolded-prerequisite-counts* (atom {})]
     ~@forms))

(defn rewrite [form]
  (with-count-atom 
    (loop [loc (zip/seq-zip form)]
      (if (zip/end? loc)
	(zip/root loc)
	(recur (zip/next (cond (not (zip/branch? loc))
			       loc
			       
			       (at-folded-prerequisite? loc)
			       (replace-with-two-prerequisites__stay_put loc)
			       
			       :else loc)))))))
  
