;; -*- indent-tabs-mode: nil -*-

(ns midje.ideas.t-tabular
  (:use [midje.ideas.tabular :except [add-binding-note table-binding-maps]]
        [midje.ideas.metaconstants :only [metaconstant-symbol?]]
        [midje.error-handling.monadic]
        [midje sweet test-util]
        [ordered.map :only (ordered-map)]))

(testable-privates midje.ideas.tabular add-binding-note table-binding-maps)

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
       c  2      3       ;; need second row, else this mis-parsing  
       4  5      9))     ;; tabular will think there are 0 rows     

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
   (str ?a ?b ?c) => ?result)

 ?a  | ?b  | ?c  | ?result
 "a" | "|" | "c" | "a|c" )

(tabular
 (fact "will ignore an optional ':where' above the table"
   (str ?a ?b) => ?result)

 :where
 ?a     | ?b     | ?result
 'where | :where | "where:where") ;; just to makes sure

(tabular
 (fact "will ignore an optional 'where' above the table"
   (+ ?a ?b) => ?result)

 where
 ?a | ?b | ?result
 1  | 2  | 3)

(tabular
 (fact "can have different forms of where or pipe in the data, no problem"
   (str ?a ?b ?c ?d) => ?result)

 where
 ?a     | ?b  | ?c     | ?d | ?result
 :where | "|" | 'where | '| | ":where|where|")

;; Error handling

(after-silently
 (tabular
  (fact "A misparenthesization that results in no table is noticed."
    (tabular-forms '?forms) => '?expected
    ?forms                       ?expect
    [ fact table ]               [fact table]))
 (fact @reported => (one-of a-user-error)))

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
  (table-binding-maps (list '?a  '?b '?result
                              1   2   3), [])
    => [ (ordered-map '?a 1, '?b 2, '?result 3) ])
 
(fact "won't count as table variables any specified local symbols"
  (table-binding-maps (list '?a  
                            '?result ; it thinks of '?result as just any old symbol
                             1   
                             3), ['?result])
    => [ (ordered-map '?a '?result) (ordered-map '?a 1) (ordered-map '?a 3) ])

;; Util: validate

(tabular "can split apart fact forms with optional doc-string"
 (fact 
   (let [s "string"]
     (validate '?forms) => '?expected))
   ?forms                               ?expected
   (tabular fact table...)              [fact [table...]]
   (tabular "string" fact table...)     [fact [table...]]
   ;; Doesn't work with non-literal strings
   (tabular s fact table...)            [s [fact table...]])

(tabular (fact ?comment
           (let [line-no-free-original ?original
                 line-no-free-expected ?expected]
             (add-binding-note line-no-free-original (ordered-map '?a 'a))
             => line-no-free-expected))

         ?comment ?original ?expected

         "binding notes can be inserted"
         '(do (midje.semi-sweet/expect (a) => b)
              (do (midje.semi-sweet/expect (inc a) => M)))
         '(do (midje.semi-sweet/expect (a) => b
                                       :binding-note "{?a a}")
                      (do (midje.semi-sweet/expect (inc a) => M
                                                   :binding-note "{?a a}")))

         "fakes do not get insertions"
         '(do (midje.semi-sweet/expect (a) => b
                                       (midje.semi-sweet/fake (x) => 3)))
         '(do (midje.semi-sweet/expect (a) => b :binding-note "{?a a}"
                                       (midje.semi-sweet/fake (x) => 3)))

         "other annotations are preserved"
         '(do (midje.semi-sweet/expect (a) => b :line 33))
         '(do (midje.semi-sweet/expect (a) => b :binding-note "{?a a}" :line 33)))

(fact "binding notes are in the order of the original row - this order is maintained within the ordered-binding-map"
  (let [actual (add-binding-note '(do (expect 1 => 2))
                                          (ordered-map '?a 1, '?b 2, '?delta "0", '?result 3))
        expected '(do (expect 1 => 2 :binding-note "{?a 1, ?b 2, ?delta \"0\", ?result 3}"))]
    actual => expected))
    
