(ns as-documentation.metaconstants
  (:require [midje
             [sweet :refer :all]
             [test-util :refer :all]]))

;;;                             THE BASICS

;;; Tests that specify too much detail about data are fragile and hard to read. Metaconstants
;;; are used to describe only the minimal information about some data. Here's the way you say
;;; that properties of the data are completely irrelevant.

(fact (vector ..a.. ..b..) => [..a.. ..b..])

;; Metaconstants can also be defined with dashes:

(fact (vector --a-- --b--) => [--a-- --b--])

;; The number of dots or dashes doesn't matter. As a convenience, you're allowed to
;; mistype metaconstants. Midje won't whine about unequal metaconstants so long as the
;; you've got some number of prefix and suffix characters:

(fact (vector --a-- --b--) => [--a- ----------------------b--])

;; It's often the case that metaconstants appear in quoted lists on the right-hand-side of
;; an example. In that case, they're not really metaconstants; they're actually symbols.
;; But the comparison still works:

(fact (vector ..a.. ..b..) => '(..a.. ..b..))

;; Metaconstants that appear more than once refer to equal metaconstants. There's nothing like
;; unification going on.

(fact (vector ..a.. ..a..) =not=> [..a.. ..b..])


;;;                             METACONSTANTS AND PREREQUISITES

;; You can use metaconstants in prerequisites.

(unfinished interesting?)

(defn choose-interesting-arg [& args]
  (first (filter interesting? args)))

(fact
  (choose-interesting-arg ..first.. ..second..) => ..second..
  (provided
    (interesting? ..first..) => false
    (interesting? ..second..) => true))

;; The prerequisites above say "I know nothing about ..first.. except
;; that it's not interesting" and "All I know about ..second.. is that
;; it's interesting. This can be clearer and cheaper than conjuring up
;; some uninteresting and interesting data.


;; You may be wondering why there's both a `..` and a `--` notation. The reason is that
;; Clojure's reader will interpret a `..` metaconstant at the head of a list as a Java method
;; call. Consequently, the following will only work with dashes:

(defn my-apply [fun & args]
  (apply fun args))

(fact
  (my-apply --v-- 1 2 3) => 8
  (provided
    (--v-- 1 2 3) => 8))


;; Metaconstants can also be used in background prerequisites, like this:

(future-fact "This is a design flaw: background and 'local' prerequisites should be merged"
  (against-background (interesting? ..first..) => false
                      (interesting? ..second..) => false
                      (interesting? ..third..) => false)
  (choose-interesting-arg ..first.. ..second..) => ..second..
  (provided
    (interesting? ..second..) => true))





;;;                             METACONSTANTS AS PARTIAL DESCRIPTIONS OF MAP-LIKE OBJECTS

;; Your function might be passed a map or a record. You want to describe the minimal
;; map that will work. That is, you'll mention only the keys that are required.

(unfinished salutation)
(defn fullname [person]
  (str (salutation person) (:given person) " " (:family person)))

(fact
  (fullname ...person...) => "Mr. Brian Marick"
  (provided
    ...person... =contains=> {:given "Brian", :family "Marick"}
    (salutation ...person...) => "Mr. "))

;; How useful is this, compared to just passing in a map? It has the
;; slight advantage that you've identified the role of the argument
;; ("person"). It's more verbose, though, and you'll find that there
;; are some operations that don't (yet) work on metaconstants:

(defn function-under-test [thing]
  (merge-with + thing {:counter 1}))

(future-fact "This doesn't work yet. Should it?"
  (function-under-test ..thing..) => (contains {:counter 2})
  (provided ..thing.. =contains=> {:counter 1}))

;; Metaconstans can be mentioned multiple times in a `provided` clause.
;; The key-value pairs are merged.

(fact "metaconstants can be mentioned multiple times"
  (+ (:a ..m..) (:b ..m..)) => 3
  (provided
    ..m.. =contains=> {:a 1}
    ..m.. =contains=> {:b 2}))

(fact "Later mentions take precedence over earlier"
  (+ (:a ..m..) (:b ..m..)) => 3
  (provided
    ..m.. =contains=> {:a 1, :b 333333}
    ..m.. =contains=> {:b 2}))

(fact "You can describe the metaconstant in a background prerequisite"
  (fact "Here's one within a fact"
    (against-background ..m.. =contains=> {:a 1})
    (:a ..m..) => 1)

  (against-background [--mc-- =contains=> {:b 20}]
    (fact "Here's one around a fact"
      (:b --mc--) => 20))

  (against-background [--mc-- =contains=> {:c 300}]
    (fact "background and local prerequisites have their key/value pairs merged"
      (against-background --mc-- =contains=> {:d 4000})
      (+ (:c --mc--) (:d --mc--) (:e --mc--)) => 54300
      (provided
        --mc-- =contains=> {:e 50000})))

  (fact "when merging happens, the more local prerequisite takes precedence"
    (against-background --mc-- =contains=> {:c 4})
    (:c --mc--) => 50000
    (provided
      --mc-- =contains=> {:c 50000})))

(fact "Metaconstant implementations of Counted/IPersistentColleciton/Seq"
  (prerequisites ..thing.. =contains=> {:name "basti" :counter 1})
  (empty ..thing..) => {}
  (count ..thing..) => 2
  (seq ..thing..) => (list [:name "basti"] [:counter 1]))

(unfinished gen-doc)
(fact "Test merging of metaconstant that appear in data and function fakes"
  (:header (gen-doc)) => "gamma"
  (provided
    (gen-doc) => ..doc..
    ..doc.. =contains=> {:header "gamma"}))

(fact "Test merging of nested metaconstant that appear in data and function fakes"
  (:header (:raw (gen-doc))) => "gamma"
  (provided
    (gen-doc) => {:raw ..doc..}
    ..doc.. =contains=> {:header "gamma"}))

(against-background [..doc.. =contains=> {:header "gamma"}]
  (fact "Test merging of metaconstant with against-background"
    (:header (gen-doc)) => "gamma"
    (provided
      (gen-doc) => ..doc..)))

(against-background [..doc..    =contains=> {:header ..header..}
                     ..header.. =contains=> {:title "title"}]
  (future-fact "Test metaconstants that contain other metaconstants"
    (-> (gen-doc) :header :title) => "title"
    (provided
      (gen-doc) => ..doc..)))

(future-fact "Test metaconstants that contain other metaconstants"
  (-> (gen-doc) :header :title) => "title"
  (provided
    (gen-doc) => ..doc..
    ..doc..    =contains=> {:header ..header..}
    ..header.. =contains=> {:title "title"}))

(future-fact "Metaconstant merging and streaming"
  (vector (:header (gen-doc)) (:header (gen-doc))) => [1 2]
  (provided
    (gen-doc) =streams=> [..doc1.. ..doc2..]
    ..doc1.. =contains=> {:header 1}
    ..doc2.. =contains=> {:header 2}))
