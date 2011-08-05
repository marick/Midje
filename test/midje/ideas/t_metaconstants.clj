;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-metaconstants
  (:use midje.ideas.metaconstants
        [midje sweet test-util]
        clojure.pprint)
  (:require [clojure.zip :as zip])
  (:import midje.ideas.metaconstants.Metaconstant))

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


(fact "Metaconstants implement ILookup"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (:key mc) => "value"
    (:not-key mc "default") => "default"
    "And let's allow the other type of map lookup"
    (mc :key) => "value"
    (mc :not-key "default") => "default"))

(fact "Metaconstants implement Associative"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (contains? mc :key) => truthy
    (contains? mc :not-key) => falsey

    (find mc :key) => [:key "value"]

    (let [new-mc (assoc mc :new-key "new value")]
      (type new-mc) => Metaconstant
      (:new-key new-mc) => "new value"
      "Note that the new metaconstant is equal to the old!"
      (= new-mc new-mc))))
    
(fact "Associate extends Seqable and IPersistentCollection"
  (let [mc (Metaconstant. 'm {:key "value"})]
    (seq mc) => (seq {:key "value"})
    (count mc) => 1
    (empty? mc) => falsey
    (.equiv mc mc) => truthy))


(fact "From a user level, what works and doesn't work with metaconstants"
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc."

  "all three types of lookup"
  (:a ..mc..) => 5
  (provided
    ..mc.. =contains=> {:a 5})
  (get ..mc.. :a) => 5
  (provided
    ..mc.. =contains=> {:a 5})
  "Note, reader reads (. ..mc..) as (. .mc..), hence convoluted code below."
  (apply ..mc.. [5]) => "five"
  (provided
    ..mc.. =contains=> {5 "five"})

  "Equality works, but I'm skeptical that using it on two objects that
   are supposed to be partial descriptions is really a good idea."
  (= ..m.. ..n..) => truthy 
  (provided
    ..m.. =contains=> {:a 3}
    ..n.. =contains=> {:a 3})
  ;; See below for extended equality

  "assoc works. The result type is a map though,
   unlike assoc on records.  It'd be creepy to have two things that
   print the same be different."
  (assoc ..m.. 'b odd?) => {'a even?, 'b odd?}
  (provided
    ..m.. =contains=> {'a even?})
  (assoc ..m.. 'b odd?) => map?
  (provided
    ..m.. =contains=> {'a even?})

  "merge works. As with assoc, the result is a hash."
  (merge ..m.. ..n..) => {:a 3, :b 4}
  (provided
    ..m.. =contains=> {:a 3}
    ..n.. =contains=> {:b 4})
  
  (merge ..m.. ..n..) => map?
  (provided
    ..m.. =contains=> {:a 3}
    ..n.. =contains=> {:b 4})

  "keys, values, and contains"
  (keys ..m..) => [:a :b]
  (provided
    ..m.. =contains=> {:a 3, :b 4})
  (vals ..m..) => [3 4]
  (provided
    ..m.. =contains=> {:a 3, :b 4})
  (contains? ..m.. :a) => truthy
  (provided
    ..m.. =contains=> {:a 3})
  (contains? ..m.. :c) => falsey
  (provided
    ..m.. =contains=> {:a 3})

  
  )

(defrecord NoAssocRecord [a b])
(fact "comparing a metaconstant using a just or contains"
  {:a even? :b odd?} => {:a even?, :b odd?}
  (NoAssocRecord. even? odd?) => {:a even?, :b odd?}
  ..m.. => {:a even?, :b odd?}
  (provided ..m.. =contains=> {:a even? :b odd?})

  {:a even? :b odd?} => (just {:a (exactly even?), :b (exactly odd?)})
  (NoAssocRecord. even? odd?) => (just {:a (exactly even?), :b (exactly odd?)})
  ..m.. => (just {:a (exactly even?), :b (exactly odd?)})
  (provided ..m.. =contains=> {:a even? :b odd?})

  {:a even? :b odd?} => (contains {:a (exactly even?), :b (exactly odd?)})
  (NoAssocRecord. even? odd?) => (contains {:a (exactly even?), :b (exactly odd?)})
  ..m.. => (contains {:a (exactly even?), :b (exactly odd?)})
  (provided ..m.. =contains=> {:a even? :b odd?})

  "Map, reduce"
  (map (fn [[_ value]] value) ..m..) => (just #{1 2 3})
  (provided
    ..m.. =contains=> {:a 1, :b 2, :c 3})
  (reduce (fn [so-far [_ value]] (+ so-far value))
          0
          ..m..) => 6
  (provided
    ..m.. =contains=> {:a 1, :b 2, :c 3})

  
)


(defn concer [source] (str (:a source) (:b source) (:c source)))
(fact
  (let [c 'c]
    (concer ...source...) => "abc"
    (provided
      ...source... =contains=> '{:a a, :b b}
      ...source... =contains=> {:c c})))
  


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

