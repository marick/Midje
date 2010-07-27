(ns midje.sweet
  (:use clojure.test
        [clojure.contrib.ns-utils :only [immigrate]]
	clojure.contrib.error-kit)
)
(immigrate 'midje.unprocessed)
(immigrate 'midje.semi-sweet)

(deferror odd-test-forms [] [forms]) 
  

(defn- parse-forms 
  ([forms] (parse-forms forms []))
  ([forms so-far] 
   (cond 
    (empty? forms) 
    so-far

    (= (second forms) '=>)
    (let [[call-form _ expected-result & remainder] forms
	  this-result { :function-under-test call-form
	  :expected-result expected-result }
	  so-farther (conj so-far this-result)]
      (parse-forms remainder so-farther))

    (= (and (seq? forms)
	    (= (first forms) 'provided)))
    (do 
      (println "To handle -- provided")
      so-far)

    :else
    (do (println "Problem with " + forms)
	(raise odd-test-forms forms)))
   )
)

(defn make-expect-call [parsed-form]
  `(expect ~(:function-under-test parsed-form) => ~(:expected-result parsed-form)))

(defmacro fact [& forms]
  (let [parsed-forms (parse-forms forms)
	expect-calls (map make-expect-call parsed-forms)]
    `(do ~@expect-calls))
  )

(defmacro facts [& forms]
  `(fact ~@forms))

