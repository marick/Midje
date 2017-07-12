(ns midje.data.t-metaconstant
  (:require [midje.data.metaconstant :refer :all]
            [midje
             [sweet :refer :all]
             [test-util :refer :all]]
            [clojure.zip :as zip])
  (:import midje.data.metaconstant.Metaconstant))

;;; Metaconstant symbols

(tabular
  (fact "metaconstant symbols begin and end with dots"
    '?candidate   ?arrow metaconstant-symbol?)
     ?candidate   ?arrow
     ...foo...      =>
     .foo.          =>
     foo            =not=>
     .foo           =not=>
     foo.           =not=>
     ".string."     =not=>
     (..foo..)      =not=>
     ...            =not=>)

(tabular "or they begin and end with dashes"
  (fact
    '?candidate   ?arrow metaconstant-symbol?)
     ?candidate   ?arrow
     ---foo---      =>
     -foo-          =>
     -a-b-          =>
     foo            =not=>
     -foo           =not=>
     foo-           =not=>
     "-string-"     =not=>
     (--foo--)      =not=>
     ;; "..." is too potentially valid to be accepted as a metaconstant,
     ;; but "---" seems useless enough that we'll allow it to be a
     ;; (badly chosen) metaconstant.
     ---            =>)

(fact "but they must be exclusively one or the other"
    (metaconstant-symbol? '-x.) => false
    (metaconstant-symbol? '.x-) => false)



;;; Metaconstants

(let [mc (Metaconstant. '..name.. {} nil)]
  (fact "Metaconstants print as their name"
    (str mc) => "..name.."
    (pr-str mc) => "..name.."))

(fact "Metaconstants implement Named"
  (name (Metaconstant. '..name. {} nil)) => "..name.")

(fact "Metaconstants are equal if their names are *comparable*."
  (fact "equal names are comparable"
    (Metaconstant.    '...name... {} nil) => (Metaconstant. '...name... {} nil)
    (Metaconstant.    '...name... {} nil) =not=> (Metaconstant. '...other... {} nil))

  (fact "but so are names that have a different number of dots or dashes"
    (Metaconstant.    '...name... {} nil) => (Metaconstant. '.name. {} nil)
    (Metaconstant.    '---name- {} nil) => (Metaconstant. '-name--- {} nil))

  (fact "However, dot-names are not equal to dash-names"
    (Metaconstant.    '...name... {} nil) =not=> (Metaconstant. '---name--- {} nil))

  (fact "values are irrelevant"
    (Metaconstant.    '...name... {:key "value"} nil) => (Metaconstant. '...name... {:key "not-value"} nil)
    (Metaconstant.  '...NAME... {:key "value"} nil) =not=> (Metaconstant. '...name... {:key "value"} nil))

  (fact "Metaconstants are equal to symbols with a comparable name"
    (= (Metaconstant. '...name... {} nil) '.name.) => truthy
    (= (Metaconstant. '...name... {} nil) '...not-name...) => falsey

    (fact "which means they can be compared to quoted lists"
      (list 'a (Metaconstant. '...name... {} nil)) => '(a ...name.)
      ;; The following works because Clojure shifts Associates to left-hand-side
      '(a ...name...) => (list 'a (Metaconstant. '...name... {} nil)))))

(fact "Metaconstants implement ILookup"
  (let [mc (Metaconstant. 'm {:key "value"} nil)]
    (:key mc) => "value"
    (:not-key mc "default") => "default"
    "And let's allow the other type of map lookup"
    (mc :key) => "value"
    (mc :not-key "default") => "default"))

(fact "Metaconstants implement Associative lookup"
  (let [mc (Metaconstant. 'm {:key "value"} nil)]
    (contains? mc :key) => truthy
    (contains? mc :not-key) => falsey

    (find mc :key) => [:key "value"]))

(fact "Associate extends some of Seqable and IPersistentCollection"
  (let [mc (Metaconstant. 'm {:key "value"} nil)]
    (count mc) => 1
    (empty? mc) => falsey
    (.equiv mc mc) => truthy))

(facts "Metaconstants implement IObj"
  (let [mc (Metaconstant. 'm {} nil)]
    (meta mc) => nil
    (.meta (with-meta mc {:key "value"})) => {:key "value"})
  (let [mc (Metaconstant. 'm {} {:key "value"})]
    (meta mc) => {:key "value"}
    (.meta (with-meta mc {:key "other"})) => {:key "other"}))

(fact "metaconstants print funny"
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc.")


;;;  The following are essentially the same tests, except with the in-fact metaconstant notation.

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
  (keys ..m..) => (just [:a :b] :in-any-order)
  (vals ..m..) => (just [3 4] :in-any-order)
  (contains? ..m.. :a) => truthy
  (contains? ..m.. :c) => falsey)

(fact "Map, reduce"
  (against-background ..m.. =contains=> {:a 1, :b 2, :c 3})
  (map (fn [[_ value]] value) ..m..) => (just #{1 2 3})
  (reduce (fn [so-far [_ value]] (+ so-far value))
          0
          ..m..) => 6)

;;; Metaconstant behavior

(unfinished gen-doc)
(fact "=contains=> is evaluated once, even when referenced multiple times"
  (identical? (gen-doc) (gen-doc)) => truthy
  (provided
    ..doc.. =contains=> {:header (rand)}
    (gen-doc) => ..doc..))

