(ns midje.sweet.background
  (use [midje.sweet.sweet-to-semi-sweet-rewrite
	:only [one-fake-body-content
	       make-fake
	       ]]))

(defn is-arrow-form? [forms]
  (= (str (second forms)) "=>"))

(defn make-background [fake]
  (concat fake '(:type :background)))

(defn parse-state [form] [ [] [] ])

(defn separate [forms]
  (loop [in-progress forms
	 default-prerequisites []
	 once-state []
	 each-state []
	 other-forms []]
    (cond (empty? in-progress)
	  [default-prerequisites once-state each-state other-forms]

	  (and (list? (first in-progress))
	       (= (str (ffirst in-progress)) "state"))
	  (let [ [new-once-state new-each-state] (parse-state (first in-progress))]
	       (recur (rest in-progress)
		      default-prerequisites
		      (concat once-state new-once-state)
		      (concat new-each-state new-each-state)
		      other-forms))
	  
	  (is-arrow-form? in-progress)
	  (let [content (one-fake-body-content in-progress)]
	    (recur (nthnext in-progress (count content))
		   (conj default-prerequisites (-> content make-fake make-background))
		   once-state
		   each-state
		   other-forms))
	  
	  :else
	  (recur (rest in-progress)
		 default-prerequisites
		 once-state
		 each-state
		 (conj other-forms (first in-progress))))))
