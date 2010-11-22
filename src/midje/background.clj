(ns midje.background
  (:use [midje.util.thread-safe-var-nesting :only [set-namespace-value]])
  (:use midje.util.recognizing-forms)
  (:require [me.fogus.unifycle :as unify]))


(defn background-fakes [] (:midje/background-fakes (meta *ns*)))

(defn set-background-fakes [newval]
  (set-namespace-value :midje/background-fakes newval))

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

(defn make-background [fake]
  (concat fake '(:type :background)))

(defn background-form? [form]
  (form-first? form "against-background"))

(defn separate-fact [fact-forms]
  [ (apply concat (map rest (filter background-form? fact-forms)))
    (filter (comp not background-form?) fact-forms) ])
  
(defn wrap [outer-form inner-form]
  (unify/subst outer-form {'?form inner-form}))
