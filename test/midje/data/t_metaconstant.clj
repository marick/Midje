(ns midje.data.t-metaconstant
  (:use midje.data.metaconstant
        [midje sweet test-util]
        clojure.pprint)
  (:require [clojure.zip :as zip])
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
     foo            =not=>
     -foo           =not=>
     foo-           =not=>
     "-string-"     =not=>
     (--foo--)      =not=>
     ---            =not=>)

(fact "but they must be exclusively one or the other"
    (metaconstant-symbol? '-x.) => false)



;;; Metaconstants

(let [mc (Metaconstant. '..name.. {})]
  (fact "Metaconstants print as their name"
    (str mc) => "..name.."
    (pr-str mc) => "..name.."))

(fact "Metaconstants implement Named"
  (name (Metaconstant. '..name. {})) => "..name.") 

(fact "Metaconstants are equal if their names are *comparable*."
  (fact "equal names are comparable"
    (Metaconstant.    '...name... {}) => (Metaconstant. '...name... {})
    (Metaconstant.    '...name... {}) =not=> (Metaconstant. '...other... {}))
  
  (fact "but so are names that have a different number of dots or dashes"
    (Metaconstant.    '...name... {}) => (Metaconstant. '.name. {})
    (Metaconstant.    '---name- {}) => (Metaconstant. '-name--- {}))
  
  (fact "However, dot-names are not equal to dash-names"
    (Metaconstant.    '...name... {}) =not=> (Metaconstant. '---name--- {}))
    
  (fact "values are irrelevant"
    (Metaconstant.    '...name... {:key "value"}) => (Metaconstant. '...name... {:key "not-value"})
    (Metaconstant.  '...NAME... {:key "value"}) =not=> (Metaconstant. '...name... {:key "value"}))
  
  (fact "Metaconstants are equal to symbols with a comparable name"
    (= (Metaconstant. '...name... {}) '.name.) => truthy
    (= (Metaconstant. '...name... {}) '...not-name...) => falsey

    (fact "which means they can be compared to quoted lists"
      (list 'a (Metaconstant. '...name... {})) => '(a ...name.)
      ;; The following works because Clojure shifts Associates to left-hand-side
      '(a ...name...) => (list 'a (Metaconstant. '...name... {})))))

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

(fact "metaconstants print funny"
  (str .mc.) => ".mc."
  (pr-str .mc.) => ".mc.")


