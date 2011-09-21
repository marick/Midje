;; -*- indent-tabs-mode: nil -*-

(ns midje.internal-ideas.t-fakes
  (:use [midje sweet test-util]
        midje.internal-ideas.fakes
        [midje.ideas.metaconstants :only [metaconstant-for-form]])
  (:import midje.ideas.metaconstants.Metaconstant))


(tabular
 (facts "the arg matcher maker hadles functions specially"
   ((arg-matcher-maker ?expected) ?actual) => ?result)
 ?expected              ?actual         ?result
 1                      1               TRUTHY
 1                      odd?            falsey

 anything               3               TRUTHY
 anything               odd?            TRUTHY
 (roughly 3)            3               TRUTHY
 (roughly 3)            0               falsey
 (contains 1)           [3 1]           TRUTHY
 (contains 1)           [3 3]           falsey
 (contains odd?)        [3]             TRUTHY
 (contains odd?)        [2 odd?]        falsey

 (exactly odd?)         odd?            TRUTHY
 (exactly odd?)         3               falsey

 (as-checker odd?)      odd?            falsey
 (as-checker odd?)      3               TRUTHY

 odd?                   odd?            TRUTHY
 odd?                   3               falsey)

 

(declare f g)
(fact "unique variables can be found in fakes"
  (let [fakes [ (fake (f 1) => 2)
                (fake (f 2) => 4)
                (fake (g) => 3)] ]
    (unique-vars fakes) => (contains [#'f #'g] :in-any-order))
  "Same applies to data-fakes"
  (let [fakes [ (data-fake ...f... => {:a 2})
                (data-fake ...f... => {:b 4})
                (data-fake ...g... => {:d 4})] ]
    (unique-vars fakes) => (contains [#'...f... #'...g...] :in-any-order)))
  

(tabular
 (facts "matching calls depend on both function name and arguments"
   (let [fake {:lhs 'expected, :arg-matchers [ odd? ] }]
     (find-matching-call ?faked-fun ?args [fake]) ?arrow fake))

 ?faked-fun     ?args   ?arrow
 'not-expected  [3]     =not=>  ; function name
 'expected      [3 3]   =not=>  ; arg count
 'expected      [4]     =not=>  ; arg value
 'expected      [3]     =>)

(fact "fakes keep track of their call counts"
  (let [fakes [(fake (f 1) => 3)
               (fake (g 1) => 4)
               (fake (f 2) => 5)]
        counts #(map fake-count fakes)]
    (call-faker #'f [1] fakes)    (counts) => [1 0 0]
    (call-faker #'f [1] fakes)    (counts) => [2 0 0]
    (call-faker #'f [2] fakes)    (counts) => [2 0 1]
    (call-faker #'g [1] fakes)    (counts) => [2 1 1]))

(fact "binding maps contain functions that increment a call count"
  (let [fake (fake (f 1) => 3)
        result-map (binding-map [fake])]
    ( (result-map #'f) 1) => 3
    (fake-count fake) => 1))

(fact "binding maps can also contain Metaconstants to assign"
  (let [data-fakes [(data-fake ...mc... =contains=> {:a 1, :b ...even...})
                    (data-fake ...mc... =contains=> {:c inc})]
        result-map (binding-map data-fakes)]
    (.storage (result-map #'...mc...)) => {:a 1, :b ...even..., :c inc}))

(fact "Unintuitively, earlier binding maps override later"
  (let [fakes [(fake (f 1) => 3 :type :background)
               (fake (f 1) => 4 :type :background)]
        result-map (binding-map fakes)]

    ( (result-map #'f) 1) => 3
    (map fake-count fakes) => [1 0]))




(tabular (fact "The number of calls can be described"
           (let [fake-fake {:count-atom (atom ?actual-count),
                            :type ?type
                            :times ?specified-count}]
           (call-count-incorrect? fake-fake) => ?expected))

         ?type          ?actual-count   ?specified-count        ?expected
         :fake          0               nil                     TRUTHY
         :fake          1               nil                     falsey
         :fake          200             nil                     falsey

         :fake          2              (range 0 2)              TRUTHY
         :fake          1              (range 0 2)              falsey
         :fake          1              #{0 2}                   TRUTHY

         :fake          1              2                        TRUTHY
         :fake          1              1                        falsey

         :fake          1               even?                   TRUTHY
         :fake          2               even?                   falsey)


         

(defn called-because-mock-checking-requires-it [] nil)
(defn has-faked-function []
  (called-because-mock-checking-requires-it)
  (implements-a-fake? called-because-mock-checking-requires-it))
         
(fact "A faked function can be identified from its metadata"
  (has-faked-function) => falsey
  (has-faked-function) => truthy
  (provided
    (called-because-mock-checking-requires-it) => 33))
  
(facts "about result suppliers used"
  "returns identity for =>"
  (let [arrow "=>"]
    ((make-result-supplier arrow [1 2 3])) => [1 2 3])
             
  "returns stream for =streams=>"
  (let [supplier (make-result-supplier "=streams=>" [1 2 3])]
    (supplier) => 1
    (supplier) => 2
    (supplier) => 3))
                    



;; Folded fakes

(defmacro some-macro [& rest] )


(tabular 
 (fact "things that are not fake-sexps don't need to be unfolded"
   ?thing ?arrow folded-fake?)

  ;; things not a proper fake macro
  ?thing                                        ?arrow
  '1                                            =not=> 
  '()                                           =not=> 
  '(fake (f (h 1)))                             =not=> ; not in right namespace
  '(midje.semi-sweet/non-fake (f (h 1)))        =not=>

  ;; Sad but true: a cons is not a list.
  (cons 'midje.semi-sweet/fake '((f (h 3)) =test=> 3))    => )

(tabular
 (fact "unfolding depends on the inner structure of a funcall"
  '(midje.semi-sweet/fake ?call =test=> 3) ?arrow folded-fake?)
   
 ?call                  ?arrow
 ;; Things that might be misinterpreted as nested funcalls
  (f)                  =not=> 
  (f 1)                =not=> 
  (f 1 '(foo))         =not=> 
  (f 1 [foo])          =not=> 
  (f 1 {foo 1})        =not=> 

  ;; These are real nested function calls
  (f (h 1))              => 
  (f 1 (h 1))            => 

  ;; but don't decide to unfold a checker used as argument matcher"
  (f 1 (exactly even?))  =not=>

  ;; don't unfold a constructor.
  (f (java.util.Date. 1 1 1))           =not=>
  (f (new java.util.Date 1 2 2))        =not=>
  
  "Macros are surprisingly hard to get right"
;  '(f 1 (some-macro 33))  =not=> folded-fake?
  )
;; unfolding prerequisites
(facts "about each step of unfolding"
  "unfolding a non-fake just moves the head of the list"
  (unfolding-step '[...] '[blah]   {}) => [ '[... blah]   [] {} ]
  (unfolding-step '[...] '[(blah)] {}) => [ '[... (blah)] [] {} ]

  "unfolding a plain fake does nothing in particular"
  (unfolding-step '[...] '[(midje.semi-sweet/fake (f 1) =test=> 4) ...] {})
  => [ '[... (midje.semi-sweet/fake (f 1) =test=> 4)] '[...] {} ]

  "unfolding a fake that should be unfolded adds a new fake"
  (let [original '(midje.semi-sweet/fake (f (h 1)) =test=> 4 ...overrides...)
        flattened '(midje.semi-sweet/fake (f ...h-1...) =test=> 4 ...overrides...)
        generated '(midje.semi-sweet/fake (h 1) => ...h-1... ...overrides...)]
    (unfolding-step '[...]
                    [original '...]
                    {})
    => [ ['... flattened]
         [generated '...]
         '{(h 1) ...h-1...} ]
    (provided
      (augment-substitutions {} original) => '{(h 1) ...h-1...}
      (flatten-fake original '{(h 1) ...h-1...}) => flattened
      (generate-fakes '{(h 1) ...h-1...} [...overrides...]) => [generated]))
  )

(fact "substitutions are augmented by unique nested args in fake"
  (augment-substitutions {} '(fake (f (h 1)))) => '{ (h 1) ...h-1... }
  (provided
    (metaconstant-for-form '(h 1)) => '...h-1...)
  "Which means that already-existing substitutions are reused"
  (augment-substitutions {'(h 1) ...h-1...} '(fake (f (h 1)))) => '{ (h 1) ...h-1... })

(fact "fakes are flattened by making substitutions"
  (flatten-fake '(fake (f (g 1) 2 (h 3)) =test=> 33 ...overrides...)
                '{ (g 1) ...g-1..., (h 3) ...h-1... })
  => '(fake (f ...g-1... 2 ...h-1...) =test=> 33 ...overrides...))

(fact "generated fakes maintain overrrides"
  (let [g-fake '(midje.semi-sweet/fake (g 1) midje.semi-sweet/=> ...g-1... ...overrides...)
        h-fake '(midje.semi-sweet/fake (h 3) midje.semi-sweet/=> ...h-1... ...overrides...)]
    (set (generate-fakes '{ (g 1) ...g-1..., (h 3) ...h-1... } '(...overrides...)))
    => #{g-fake h-fake}))


;;; Internal functions


(fact "data-fakes can be converted to metaconstant-bindings"
  (let [bindings (data-fakes-to-metaconstant-bindings [{:lhs #'name :contained {:a 1}}])
        _ (fact (count bindings) => 1)
        binding (first bindings)
        _ (fact (count binding) => 1)
        [var metaconstant] (first binding)]
    (.name metaconstant) => 'name
    (.storage metaconstant) => {:a 1}))

(declare var-for-merged var-for-irrelevant)
(fact "metaconstant bindings can have their values merged together"
  (let [first-half (Metaconstant. 'merged {:retained 1, :replaced 2})
        second-half (Metaconstant. 'merged {:replaced 222, :extra 3})
        irrelevant (Metaconstant. 'irrelevant {:retained :FOO :extra :BAR})
        all [{#'var-for-merged first-half} {#'var-for-merged second-half} {#'var-for-irrelevant irrelevant}]
        result (merge-metaconstant-bindings all)]
    
    (.storage (result #'var-for-merged)) => {:retained 1, :replaced 222, :extra 3}
    (.storage (result #'var-for-irrelevant)) => {:retained :FOO, :extra :BAR}))

