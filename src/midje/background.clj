(ns midje.background
  (:use [midje.util.thread-safe-var-nesting :only [set-namespace-pseudovariable]])
  (:use midje.util.forms)
  (:use [midje.sweet.sweet-to-semi-sweet-rewrite
	 :only [one-fake-body-content make-fake]])
  (:require [me.fogus.unifycle :as unify]))


(defn background-fakes [] (:midje/background-fakes (meta *ns*)))

(defn set-background-fakes [newval]
  (set-namespace-pseudovariable :midje/background-fakes newval))

(defn push-background-fakes [fakes]
  (set-background-fakes (cons (reverse fakes) (background-fakes))))

(defn pop-background-fakes [] 
  (set-background-fakes (rest (background-fakes))))

(defmacro with-background-fakes [fakes & forms]
  "Check forms with fakes established as a 'background' -- they will
   be used if needed, but it's not a failure if they're unused."
  `(try
     (push-background-fakes ~fakes)
     ~@forms
     (finally (pop-background-fakes))))

(defn background-fakes-plus [fakes]
  (flatten (cons fakes (background-fakes))))

(defn is-arrow-form? [forms]
  (= (str (second forms)) "=>"))

(defn make-background [fake]
  (concat fake '(:type :background)))

(defn expand [forms]
  (loop [expanded []
	 in-progress forms]
    (cond (empty? in-progress)
	  expanded

	  (is-arrow-form? in-progress)
	  (let [content (one-fake-body-content in-progress)]
	    (recur (conj expanded (-> content make-fake make-background))
		   (nthnext in-progress (count content))))
	  
	  :else
	  (throw (Error. "This doesn't look like part of a background:" in-progress)))))

(defn background-form? [form]
  (form-first? form "against-background"))

(defn separate-fact [fact-forms]
  [ (apply concat (map rest (filter background-form? fact-forms)))
    (filter (comp not background-form?) fact-forms) ])
  
(defn wrap [outer-form inner-form]
  (unify/subst outer-form {'?form inner-form}))
