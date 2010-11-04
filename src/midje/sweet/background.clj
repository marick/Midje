(ns midje.sweet.background
  (use [midje.sweet.sweet-to-semi-sweet-rewrite
	:only [one-fake-body-content
	       make-fake
	       ]]))

(defn is-arrow-form? [forms]
  (= (str (second forms)) "=>"))

(defn make-background [fake]
  (concat fake '(:type :background)))

(defn separate [forms]
  (loop [in-progress forms
	 default-prerequisites []
	 other-forms []]
    (cond (empty? in-progress)
	  [default-prerequisites other-forms]
	  
	  (is-arrow-form? in-progress)
	  (let [content (one-fake-body-content in-progress)]
	    (recur (nthnext in-progress (count content))
		   (conj default-prerequisites (-> content make-fake make-background))
		   other-forms))
	  
	  :else
	  (recur (rest in-progress)
		 default-prerequisites
		 (conj other-forms (first in-progress))))))
