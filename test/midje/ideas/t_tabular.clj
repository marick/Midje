(ns midje.ideas.t-tabular
  (:use [midje.ideas.tabular :except [add-binding-note table-binding-maps]]
        [midje.ideas.metaconstants :only [metaconstant-symbol?]]
        [midje.error-handling.validation-errors]
        [midje sweet test-util]
        [ordered.map :only (ordered-map)]
        midje.util)
  (:require [midje.ideas.facts :as facts]))

(expose-testables midje.ideas.tabular)

;; Core midje.sweet API

(tabular
 (fact (+ ?a ?b) => ?result )
 ?a    ?b      ?result
 1     2       3)

(tabular
 (fact "some information about that"
   (+ ?a ?b) => ?result)
 ?a    ?b      ?result
 1     2       3)

(tabular "no longer need to prefix table variables with '?'"
 (fact (+ a b) => result )

     a     b       result
     1     2       (fn [actual] (= 3 actual))
     3     4       (as-checker odd?)
     2     1       (as-checker (fn [actual] (= 3 actual))))

(let [c 1]
  (tabular "considers local bindings to be table values, not headings"
    (fact (+ a b) => result )
  
       a  b      result
       c  2      3       ;; need second row, else (in the failure case) this  
       4  5      9))     ;; mis-parsing tabular will think there are 0 rows     

(defn f? [x] true)

(tabular "won't consider bound resolvable fns as table heading var"
  (fact (+ a b) => result)    
     result  a   b
     f?      1   2    ;; needs second row, like above 
     17      8   9)
                      
(tabular "won't think of metaconstants as unbound symbols, and thus 
          won't try to make them table heading variables"
  (fact 'candidate arrow metaconstant-symbol?)
     candidate   arrow
     ---foo---     => 
     -foo-         => )

(tabular "The tabular form can have a doc string"
 (fact 
   (+ ?a ?b) => ?result)
 ?a    ?b      ?result
 1     2       3)

(tabular
 (fact "will ignore optional pipes separating table columns"
   (str a b c) => result)

 a   | b   | c   | result
 "a" | "|" | "c" | "a|c" )

(tabular
 (fact "will ignore an optional ':where' above the table"
   (str a b) => result)

 :where
 a      | b      | result
 'where | :where | "where:where") ;; just to makes sure

(tabular
 (fact "will ignore an optional 'where' above the table"
   (+ a b) => result)

 where
 a  |  b | result
 1  | 2  | 3)

(tabular
 (fact "can have different forms of where or pipe in the data, no problem"
   (str a b c d) => result)

 where
 a      | b   | c      | d  | result
 :where | "|" | 'where | '| | ":where|where|")


;; Table Validation

(each-causes-validation-error #"There's no table\. \(Misparenthesized form\?\)"
  (tabular
    (fact 
      (tabular-forms '?forms) => '?expected
      ?forms                       ?expect
      [ fact table ]               [fact table]))

  (tabular
    (fact nil => nil))

  (tabular "doc string present"
    (fact nil => nil)))

(causes-validation-error #"It looks like the table has headings, but no values:"
  (tabular
    (fact ?a => ?b)
    ?a   ?b))

(causes-validation-error #"It looks like the table has no headings"
  (tabular
    (fact
      (+ a b) => result)
      2    4   999     ))

;; Other tests via midje.sweet API

(unfinished g)
(defn f [n] (inc (g n)))

(tabular
 (fact
   (f ?n) => ?result
   (provided
     (g ?n) => ?intermediate))
 ?result ?n ?intermediate
 2       1      1
 (+ 1 2) 1      2
 3       2      2)

                                     
(tabular
 (fact "only two numbers have the same sum and square"
   (* ?n ?n) ?arrow (+ ?n ?n))
 ?n        ?arrow
 0         =>      
 2         =>
 ;; Failure cases
 1         =not=>
 (* 10 10) =not=>
 ;; and so on
 )

(defn alive? [cell-status neighbor-count]
  (cond (= cell-status :dead)
        (= 3 neighbor-count)

        :else
        (some #{neighbor-count} #{2 3})))

(tabular
  (fact "The rules of Conway's life"
    (alive? ?cell-status ?neighbor-count) => ?expected)

  ?cell-status   ?neighbor-count   ?expected
  :alive         1                 FALSEY        ; underpopulation
  :alive         2                 truthy       
  :alive         3                 truthy
  :alive         4                 FALSEY        ; overpopulation
  
  ;; A newborn cell has three parents
  :dead          2                 FALSEY
  :dead          3                 truthy
  :dead          4                 FALSEY)

(tabular
 (fact "nice fact properties are retained"
   (let [a 1]
     (f ?n) => ?result
     (provided
       (g ?n) => ?intermediate)))
 ?result ?n ?intermediate
 (+ a 1)       1      1)


(after-silently
 (tabular
  (future-fact (inc ?int) => ?int)
  ?int
  1)
 (fact @reported => (just (contains {:type :future-fact}))))



;; Util: table-binding-maps
 
(fact "gets the bindings off fact table"
  (table-binding-maps ['?a  '?b '?result] [1 2 3])
  => [ (ordered-map '?a 1, '?b 2, '?result 3) ])

(defn as-received-by-add-binding-note [body]
  (list (facts/convert-expanded-body-to-compendium-form
         body
         {:some-fact-metadata true}
         'symbol-to-name-function-with)))


(tabular (fact ?comment
           (let [line-no-free-original ?original
                 line-no-free-expected ?expected]
             (add-binding-note line-no-free-original (ordered-map '?a 'a))
             => line-no-free-expected))

         ?comment ?original ?expected

         "binding notes can be inserted"
         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b)
               (do (midje.semi-sweet/expect (inc a) => M))))
          
         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b :binding-note "[?a a]")
               (do (midje.semi-sweet/expect (inc a) => M :binding-note "[?a a]"))))

         "fakes do not get insertions"
         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b
                                        (midje.semi-sweet/fake (x) => 3))))

         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b :binding-note "[?a a]"
                                        (midje.semi-sweet/fake (x) => 3))))

         "other annotations are preserved"
         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b :line 33)))

         (as-received-by-add-binding-note
          '(do (midje.semi-sweet/expect (a) => b :binding-note "[?a a]" :line 33))))

(fact "binding notes are in the order of the original row - this order is maintained within the ordered-binding-map"
  (let [actual (add-binding-note
                (as-received-by-add-binding-note
                 '(do (expect 1 => 2)))
                (ordered-map '?a 1, '?b 2, '?delta "0", '?result 3))
        
        expected (as-received-by-add-binding-note
                  '(do (expect 1 => 2 :binding-note "[?a 1\n                           ?b 2\n                           ?delta \"0\"\n                           ?result 3]")))]
    actual => expected))
    
;; tabular doc-string prints in report

(after-silently
  (tabular "table of results"
    (fact "add stuff"
      (+ a b) => result)
    
      a    b   result
      2    4   999     )  ;; PURPOSELY FAIL
  (fact @reported => (one-of (contains {:description ["table of results" "add stuff"]} ))))

(after-silently
  (tabular "table of results"
    (fact (+ a b) => result)
    
      a    b   result
      2    4   999     )  ;; PURPOSELY FAIL 
  (fact @reported => (one-of (contains {:description ["table of results" nil]} ))))

(after-silently
  (tabular
    (fact "add stuff"  ;; Note that this gets promoted to be with `tabular`
      (+ a b) => result)
    
      a    b   result
      2    4   999     )  ;; PURPOSELY FAIL
  (fact @reported => (one-of (contains {:description ["add stuff" nil]} ))))
