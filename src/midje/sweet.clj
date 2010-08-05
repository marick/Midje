(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms]) 

(defn- basic-frozen-run [call-form expected-result]
  { :function-under-test call-form :expected-result expected-result
    :expectations [] }
)

(def frozen-runs)

(defn- no-expectations-follow? [forms]
  (or (empty? forms)
      (not (seq? (first forms)))
      (not (= 'provided (ffirst forms))))
)

(defn- add-expectations [run forms so-far]
  (if (no-expectations-follow? forms)
    (frozen-runs forms (conj so-far run))
    (let [provided-forms (rest (first forms))
	    forms (rest forms)
	    triplets (vec (partition 3 provided-forms))]
      (frozen-runs forms (conj so-far (assoc run :expectations triplets)))))
)

(defn- frozen-runs
  ([forms] (vec (frozen-runs forms [])))
  ([forms so-far] 
   (cond 
    (empty? forms) 
    so-far

    (= (second forms) '=>)
    (let [[call-form _ expected-result & remainder] forms
	  run (basic-frozen-run call-form expected-result)]
      (add-expectations run remainder so-far))

    :else
    (do (println "Problem with " + forms)
	(raise odd-test-forms forms)))
   )
  )

(defn- form-branch? [candidate]
  ;; In the absence of seqable?
  (or (seq? candidate)
      (map? candidate)
      (vector? candidate)
      (set? candidate)))

(defn- metavar? [symbol-or-form]
  (and (symbol? symbol-or-form)
       (re-matches #"^\.+.+\.+" (name symbol-or-form))))

(defn- define-metavars [form]
  (let [metavars (filter metavar? (tree-seq form-branch? seq form))]
    (doseq [metavar metavars]
      (intern *ns* metavar (symbol metavar)))
    metavars))

(defn- make-fake-calls [frozen-run]
  (map (fn [fake-call] `(midje.semi-sweet/fake ~@fake-call))
       (frozen-run :expectations)))

(defn- make-expect-call [frozen-run]
  `(expect ~(:function-under-test frozen-run) => ~(:expected-result frozen-run)
	   ~@(make-fake-calls frozen-run)))

(defmacro fact [& forms]
  (define-metavars forms)
  (let [runs (frozen-runs forms)
	expect-calls (map make-expect-call runs)]
    `(do ~@expect-calls))
    )

(defmacro facts [& forms]
  `(fact ~@forms))

