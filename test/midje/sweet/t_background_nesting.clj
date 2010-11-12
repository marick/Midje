(ns midje.sweet.t-background-nesting
  (:use clojure.test)
  (:use [midje.sweet] :reload-all)
  (:use [midje.test-util])
  (:use clojure.contrib.pprint))

;; This is a separate file because we're making namespace-wide changes

(unfinished outermost middlemost innermost)

(defmacro in-separate-namespace [& forms]
  `(let [old-ns# *ns*]
    (try (in-ns (gensym)) ~@forms
    (finally (in-ns (ns-name old-ns#))))))

(deftest background-command-slams-new-background-in-place
  (in-separate-namespace
   (background (outermost) => 2)
   (fact (+ 1 (outermost)) => 3)
   (background (outermost) => -2)
   (fact (+ 1 (outermost)) => -1)))

(deftest background-command-is-shadowed-by-against-background
  (in-separate-namespace
   (background (outermost) => 2
	       (middlemost) => 'a)
   (against-background [ (middlemost) => 33]
		       (fact (+ (middlemost) (outermost)) => 35))))
  

(deftest three-levels-of-nesting
  (in-separate-namespace
   (background (outermost) => 2
	       (middlemost) => 'a)
   (against-background [ (middlemost) => 33
			 (innermost) => 'c]
		       (fact
			 (against-background (innermost) => 8)
			 (+ (middlemost) (outermost) (innermost)) => 43))))
