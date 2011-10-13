
(ns basic.core-test
  (:use clojure.test)
  (:use midje.sweet)
)

(defn note-expected-failure [] (println "^^^^ The previous failure was expected ^^^^"))

                       ;;; 

;; This is an example of the Midje version of a clojure.test test that would 
;; look like this:
;;      (is (= (+ 1 1) 2))
;; In Midje's preferred metaphor, such statements are facts about the
;; world, so its version looks like this:

(fact (+ 1 1) => 2)

;; You can have multiple assertions within the fact form. The function
;; has an alias, facts, for those of you who are sticklers about
;; grammar:

(facts "arithmetic"
 (+ 1 1) => 2
 (+ 1 0) => 1)

;; Notice also that you can add "doc strings" to facts. Right now, they're just ignored.

;; Failing tests should look familiar:
;;     FAIL at (core_test.clj:34)
;;     Expected: 3
;;       Actual: 4

(fact ( #(+ 1 %) 3) => 3)                                          (note-expected-failure)

;; You can use clojure.test to wrap facts in deftest. 
;; See the examples/sweet-examples/leiningen-test example and
;; http://github.com/marick/Midje/wiki/Lein-test
;; In this example, I don't wrap facts. Instead, I use Midje's
;; leiningen plugin or run the tests directly. 

;; If the right-hand side of an arrow is a function, the actual result
;; is passed as the function's single argument.

(fact ( #(+ 1 %) 3) => odd?)                                       (note-expected-failure)
;; The failing test will look slightly different:
;;     FAIL at (core_test.clj:45)
;;     Actual result did not agree with the checking function.
;;         Actual result: 4
;;     Checking function: odd?


;; If you're testing something that produces a function, use
;; (exactly):
;(fact (first [even? odd?]) => (exactly odd?))                      (note-expected-failure)

;; NOTE: the previous line is commented out because Midje prints the
;; function name nicely in Clojure 1.2. A breaking change to Clojure
;; 1.3 makes it harder to accomplish this, so an ugly name that
;; can vary between runs is printed instead.  Leaving that name in the
;; output would prevent this from being used as part of the
;; just-before-release regression test suite.

;; FAIL at (core_test.clj:56)
;; Actual result did not agree with the checking function.
;;     Actual result: a function named 'even?'
;; Checking function: (exactly odd?)

;; (Midje can't always determine the name of the function. If not, it'll print
;; gobbledeegook like #<core$even_QMARK_ clojure.core$even_QMARK_@21a722ef>.)

;; There are a number of matching functions available. You can find them all with 
;;            (ns-publics 'midje.checkers)
;; They have doc strings.
;; (Or you can look at the wiki: http://github.com/marick/Midje/wiki/Checkers)
;; Here's one of them:

(fact
  [3 1 2] => (just [1 2 3] :in-any-order)                         ;; succeeds
  [3 3 1 2] => (just [1 2 3] :in-any-order))                       (note-expected-failure)
;; FAIL at (core_test.clj:74)		
;; Actual result did not agree with the checking function.
;;         Actual result: [3 3 1 2]
;;     Checking function: (just [1 2 3] :in-any-order)
;;     The checker said this about the reason:
;;         Expected three elements. There were four.

;; You can negate the sense of a check:

(fact (set [1 2 3]) =not=> sequential?)

;; Facts work with variables bound both within and outside of the fact form:
(let [a 3]
  (fact (+ a a) => 6)
  (fact (let [b 33]
	  (+ a b) => 36)))

;; Sometimes facts are only true provided other facts are true. Midje
;; has a notation for that. Suppose we're testing the rules for
;; Conway's Life (as used in Corey Haines' Code Retreat workshops). In
;; it, a cell "comes to life" if it is dead but has three living
;; neighbors. Let's say that rule will be part of an
;; alive-in-next-generation? function. As normal, we have to declare
;; that function before using it in a fact:

(defn alive-in-next-generation? [cell]  
  ; not started yet.
)

;; As we write the fact, we'll decide on prerequisite facts, which are
;; themselves functions that need to be declared. We could use the
;; Clojure's `declare` macro, but I prefer this one:

(unfinished alive? neighbor-count)

;; It makes it a little clearer what role those vars are playing, and
;; it defines the functions to blow up informatively if they're ever
;; called. Also, the Midje source contains an Emacs minor mode to make
;; it more convenient to write facts and add prerequisites to the
;; unfinished list.  See
;; http://github.com/marick/Midje/wiki/Midje-mode

;; All that given, here's an expression of the rule that lets you get the
;; logic of alive-in-next-generation? right before fussing with
;; counting neighbor cells:

(fact
 (alive-in-next-generation? ...cell...) => truthy
   (provided 
    (alive? ...cell...) => false
    (neighbor-count ...cell...) => 3))
(println "^^^^ The previous three failures were expected ^^^^")

;; Notes:
;; ...cell... : I use this shorthand to represent some 
;;              random value. None of its characteristics
;;              matter except those defined by prerequisites.
;;              You don't have to define such "metaconstants" -
;;              Midje does it for you.
;;
;; truthy     : Truthy is another one of Midje's checking
;;              functions. It accepts any value other than
;;              false or nil.
;;
;; provided   : Provided gives a list of prerequisites the original
;;              fact depends upon. (Note that the "provided"
;;              form isn't within the original claim, but follows
;;              just after it.)  

;; When the fact is checked, it will fail like this:

;; FAIL for (core_test.clj:120)
;; You claimed the following was needed, but it was never used:
;; (alive? ...cell...)
;; FAIL for (core_test.clj:121)
;; You claimed the following was needed, but it was never used:
;; (neighbor-count ...cell...)
;; FAIL at (core_test.clj:118)
;; Actual result did not agree with the checking function.
;;     Actual result: nil
;; Checking function: truthy


;; You can mention a particular unimplemented function more than once,
;; giving it a different argument each time:

(unfinished g)

(defn g-adder [n1 n2]
  (+ (g n1) (g n2)))

(fact
 (g-adder 2 3) => 11
 (provided
  (g 2) => 4
  (g 3) => 7))

;; When looking for a matching prerequisite, Midje 1.1 uses the same rules as
;; when checking a function-under-test's actual result. Because that's
;; been found to be confusing, it'll change. What follows is future-proof
;; test-writing. 

(unfinished subfunction)
  
(defn function-under-test [value-to-pass]
  (subfunction value-to-pass))

(facts "about functions in the argument list of a prerequisite"
  "Ordinarily, functions in an argument list have to be matched exactly"
  (function-under-test odd?) => 11
  (provided (subfunction odd?) => 11)
  
  "If you want to use an ordinary function as a checker, wrap it in as-checker."
  (function-under-test 3) => 11
  (provided (subfunction (as-checker odd?)) => 11)
  
  "Predefined checkers can be used unadorned"
  (function-under-test 'blue-cow) => 11
  (provided (subfunction anything) => 11)

  (function-under-test 3.00000001) => 11
  (provided (subfunction (roughly 3.0)) => 11))
  
;; The return values of a prerequisite function don't follow the rules
;; for arguments. They're literal constant values.

;; I often find myself with one prerequisite that returns a data structure
;; that's immediately given to another one. That would look like this:

(unfinished property-of data-structure)
(defn function-under-test []
  (property-of (data-structure)))

(fact
  (function-under-test) => 3
  (provided
    (data-structure) => ...data-structure...
    (property-of ...data-structure...) => 3))
    
;; I'd use a metaconstant for the data structure because I don't care to
;; commit myself to an exact format yet.
;;
;; Because this form is so common, you're allowed to "fold" the two
;; prerequisites together:

(defn function-under-test [] 3) ; so we get failures.

(fact
  (function-under-test) => 3
  (provided
    (property-of (data-structure)) => 3))
(println "^^^^ The previous two failures were expected ^^^^")

;; Notice that one of the failures refers to a generated metaconstant,
;; ...data-structure-value-1...

