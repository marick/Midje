;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-metaconstants
  (:use midje.ideas.metaconstants
        [midje sweet test-util]
        clojure.pprint)
  (:require [clojure.zip :as zip])
  (:import midje.ideas.metaconstants.Metaconstant))


;;; Notation

(tabular 
 (fact "metaconstants begin and end with dots"
   '?candidate ?arrow metaconstant-symbol?)
 ?candidate   ?arrow
 ...foo...      => 
 .foo.          => 
 foo            =not=>
 .foo           =not=>
 foo.           =not=>
 "foo"          =not=>
 (..foo..)      =not=>)


(tabular "or they begin and end with dashes"
 (fact 
   '?candidate ?arrow metaconstant-symbol?)
 ?candidate   ?arrow
 ---foo---      => 
 -foo-          => 
 foo            =not=>
 -foo           =not=>
 foo-           =not=>
 "foo"          =not=>
 (--foo--)      =not=>)

;; This allows them to be used as function arguments.

(defn f [fun args]
  (apply fun args))

(fact (f --v-- [1 2 3]) => 8
  (provided
    (--v-- 1 2 3) => 8))


;;; About the datatype

(let [mc (Metaconstant. '...name... {})]
  (fact "Metaconstants print as their name"
    (str mc) => "...name..."
    (pr-str mc) => "...name..."))


(fact "Metaconstants are equal if their names are equal. Value is irrelevant."
  (Metaconstant.    '...name... {:key "value"}) 
  => (Metaconstant. '...name... {:key "not-value"})

  (Metaconstant.  '...NAME... {:key "value"}) 
  =not=> (Metaconstant. '...name... {:key "value"})
  
  "And they are equal to symbols that have their name"
  (= (Metaconstant. '...name... {}) '...name...) => truthy
  (= (Metaconstant. '...name... {}) '...not-name...) => falsey
  "... which has this implication:"
  (list 'a (Metaconstant. '...name... {})) => '(a ...name...)

  "And, because Clojure moves Associative elements to the left side:"
  '(a ...name...) => (list 'a (Metaconstant. '...name... {})))

(fact "Metaconstants are equal no matter how many dots/dashes they have - with exs analagous to the above fact"
  (Metaconstant. '.name.. {}) => (Metaconstant. '...name... {})
  (Metaconstant. '.NAME. {:key "value"}) =not=> (Metaconstant. '...name... {:key "value"})
  (Metaconstant. '.name.. {}) => '...name...
  (list 'a (Metaconstant. '.name. {})) => '(a ...name...)
  [.name.] => [(Metaconstant. '...name... {})]
  (Metaconstant. '-name-- {}) => (Metaconstant. '---name--- {})
  (Metaconstant. '-name-- {}) =not=> (Metaconstant. '...name... {}))

(fact "Metaconstants implement ILookup"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (:key mc) => "value"
    (:not-key mc "default") => "default"
    "And let's allow the other type of map lookup"
    (mc :key) => "value"
    (mc :not-key "default") => "default"))

(fact "Metaconstants implement Associative lookup"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (contains? mc :key) => truthy
    (contains? mc :not-key) => falsey

    (find mc :key) => [:key "value"]))
    
(fact "Associate extends some of Seqable and IPersistentCollection"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (count mc) => 1
    (empty? mc) => falsey
    (.equiv mc mc) => truthy))



;;;;

(fact "metaconstants print funny"
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc.")

;;;  Use with prerequisite functions


(unfinished m)
(defn caller [head tail]
  (m head tail))

(fact "metaconstants work even when quoted"
  (caller 'sym ...tail...) => '(sym ...tail...)
  (provided (m 'sym ...tail...) => '(sym ...tail...)))

(defn claim-symbols [symbols]
  (fact 
    (doseq [metaconstant-symbol symbols]
      (find (ns-interns *ns*) metaconstant-symbol) => truthy
      (var-get ((ns-interns *ns*) metaconstant-symbol)) => metaconstant-symbol)))

(define-metaconstants '(fact (f ...form...) => 1
                         [:in ...vec...]
                         {:in ...map...}
                         #{:in ...set...}))
(claim-symbols '(...form... ...vec... ...map... ...set...))

"Metaconstants can be declared in backgrounds"
(declare f)
(background (f ...one...) => 1 )
(against-background [ (f ...two...) => 2 ]
  (fact 
    (+ (f ...one...) (f ...two...) (f ...three...))  => 6
    (against-background (f ...three...) => 3)))
(claim-symbols '(...one... ...two... ...three...))

(fact "metaconstants can be created to stand in for an expression"
  (with-fresh-generated-metaconstant-names
    (metaconstant-for-form '(g)) => '...g-value-1...
    (metaconstant-for-form '(g)) => '...g-value-2...
    (metaconstant-for-form '(h)) => '...h-value-1...

    "Not fooled by namespaces"
    (metaconstant-for-form '(metaconstant-for-form))
    => '...metaconstant-for-form-value-1...))

;;; Metaconstants-that-contain: as used in code

(fact "all three types of lookup"
  (against-background --mc-- =contains=> {:a 5})
  (:a --mc--) => 5
  (get --mc-- :a) => 5
  (--mc-- :a) => 5)

(fact "Equality can be used to compare two metaconstants for identity"
  (let [aliased-as-function-argument ..m..]
    (= aliased-as-function-argument ..m....) => truthy)
  "Contents are not used in a comparison check."
  (= ..m.. ..n..) => falsey
  (provided
    ..m.. =contains=> {:a 4}
    ..n.. =contains=> {:a 4}))

(fact "It is an error to compare a metaconstant to a map or record."
  (= ..m.. {:a 4}) => (throws Error))

(fact "It is appropriate to compare a metaconstant to its name."
  (= '..m.. ..m..) => truthy
  (= ..m.. '..m..) => truthy
  (= '..m.. ..nnn..) => falsey
  (= ..nnn.. '..m..) => falsey

  "even if the number of .'s is not exactly the same"
  (= '..m.. ...m...) => truthy
  (= ..m.. '...m...) => truthy
  (= 'm ..m..) => falsey
  (= ..m.. 'm) => falsey)

(fact "Metaconstant equality blows up when given anything else."
  (= ..m.. "foo") => (throws Error)
  (= ..m.. :foo) => (throws Error)
  (= ..m.. 1111) => (throws Error)
  (= "foo" ..m..) => (throws Error)
  (= :foo ..m..) => (throws Error)
  (= 11111 ..m..) => (throws Error))


(fact "a good many operations are not allowed"
  (against-background ..m.. =contains=> {'a even?}
                      ..n.. =contains=> {:b 4})
  (assoc ..m.. 'b odd?) => (throws Error)
  (merge ..m.. ..n..) => (throws Error)
  (merge {:a 1} ..n..) => {:a 1, :b 4}
  (merge ..m.. {:a 1}) => (throws Error)
  (cons [:a 1] ..m..) => [ [:a 1] ['a even?] ]  ; Can't prevent.
  (conj ..m.. {:a 3}) => (throws Error))

(fact "keys, values, and contains work on metaconstants"
  (against-background ..m.. =contains=> {:a 3, :b 4})
  (keys ..m..) => [:a :b]
  (vals ..m..) => [3 4]
  (contains? ..m.. :a) => truthy
  (contains? ..m.. :c) => falsey)

(fact "Map, reduce"
  (against-background ..m.. =contains=> {:a 1, :b 2, :c 3})
  (map (fn [[_ value]] value) ..m..) => (just #{1 2 3})
  (reduce (fn [so-far [_ value]] (+ so-far value))
          0
          ..m..) => 6)

(unfinished salutation)
(defn fullname [person]
  (str (salutation person) (:given person) " " (:family person)))

(fact
  (fullname ...person...) => "Mr. Brian Marick"
  (provided
    ...person... =contains=> {:given "Brian", :family "Marick"}
    (salutation ...person...) => "Mr. "))

(defn concer [source] (str (:a source) (:b source) (:c source)))
(fact
  (let [c 'c]
    (concer ...source...) => "abc"
    (provided
      ...source... =contains=> '{:a a, :b b}
      ...source... =contains=> {:c c})))
  
;;; Metaconstants and backgrounds

(fact "background metaconstants"
  (against-background ..m.. =contains=> {:a 1})
  (:a ..m..) => 1)

(against-background [--mc-- =contains=> {:b 20}]
  (fact "against-background can provide a containership prerequisite for a metaconstant."
    (:b --mc--)) => 20)

(background --mc-- =contains=> {:c 300})
(fact "Background can contain a containership prerequisite for a metaconstant"
  (:c --mc--) => 300)

(fact "An against-background containership prerequisite takes precedence over a background one."
  (against-background --mc-- =contains=> {:c 3})
  (:c --mc--) => 3)

(fact "three sources of containership prerequisites can be combined"
  (against-background --mc-- =contains=> {:d 4000})
  (+ (:c --mc--) (:d --mc--) (:e --mc--)) => 54300
  (provided
    --mc-- =contains=> {:e 50000}))

(fact "A provided prerequisite takes precedence"
  (against-background --mc-- =contains=> {:c 4000})
  (:c --mc--) => 50000
  (provided
    --mc-- =contains=> {:c 50000}))




(fact "metaconstants can be mentioned multiple times"
  (+ (:a ..m..) (:b ..m..)) => 3
  (provided
    ..m.. =contains=> {:a 1}
    ..m.. =contains=> {:b 2})

  "Later takes precedence over the former."
  (+ (:a ..m..) (:b ..m..)) => 3
  (provided
    ..m.. =contains=> {:a 1, :b 333333}
    ..m.. =contains=> {:b 2}))
