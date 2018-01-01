(ns as-documentation.prerequisites.aaaaaa-the-basics
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))
                                ;;; The Basics

;; One development path is to work top-down and use the `provided`
;; clause to substitute prerequisite values.

(unfinished read-project-file)
(defn fetch-project-paths []
  (try
    (flatten ((juxt :test-paths :source-paths) (read-project-file)))
  (catch Error ex
    ["test"])))

(facts "about fetch-project-paths"
  (fact "returns the project file's test and source paths, in that order"
    (fetch-project-paths) => ["test1" "test2" "source1"]
    (provided
      (read-project-file) => {:test-paths ["test1" "test2"]
                              :source-paths ["source1"]}))
  (fact "returns [\"test\"] if there is no project file."
    (fetch-project-paths) => ["test"]
    (provided
      (read-project-file) =throws=> (Error. "boom!"))))


;;; A version with metaconstants. Plain metaconstants have only one
;;; property: identity. If you see a metaconstant twice in the same
;;; checkable, you know it's the same value each time.

(fact "fetch-project-paths returns the project file's test and source paths, in that order"
  (fetch-project-paths) => [..test1.. ..test2.. ..source..]
  (provided
    (read-project-file) => {:test-paths [..test1.. ..test2..]
                            :source-paths [..source..]}))


;;; A first version of a grade-point average application:

(defn gpa [courses]
  (let [credits (map :credit-hours courses)
        grades (map :grade courses)
        weighted (map * credits grades)]
    (/ (reduce + 0.0 weighted) (reduce + 0 credits))))

(fact
  (let [coursework-for-3_66-gpa [{:credit-hours 1, :grade 5}
                                 {:credit-hours 2, :grade 3}]]
    (gpa coursework-for-3_66-gpa) => (roughly 3.66 0.01)))

;;; A version that gives wealth its due.

(unfinished child-of-wealthy-alumnus?)

(defn gpa [student courses]
  (let [credits (map :credit-hours courses)
        grades (map :grade courses)
        weighted (map * credits grades)
        true-gpa (/ (reduce + 0.0 weighted) (reduce + 0 credits))
        adjustment (if (child-of-wealthy-alumnus? student) 0.5 0)]
    (+ true-gpa adjustment)))

(fact
  (let [correct-gpa 3.66
        tolerance 0.01
        coursework [{:credit-hours 1, :grade 5}
                    {:credit-hours 2, :grade 3}]]

    (gpa ..student.. coursework) => (roughly correct-gpa tolerance)
    (provided (child-of-wealthy-alumnus? ..student..) => false)

    (gpa ..student.. coursework) => (roughly (+ correct-gpa 0.5) tolerance)
    (provided (child-of-wealthy-alumnus? ..student..) => true)))


                        ;;; What happens when a check fails?

(unfinished lower-function)

(defn top-function [n]
  (+ (lower-function n) (lower-function (inc n))))



(capturing-failure-output
 (fact
   (top-function 5) => 55
   (provided
     ;; (lower-function 5) => 50    ; do not describe one of the two calls
     (lower-function 6) =>  5))
 ;; So...
 (fact
   @fact-output => #"You never said #'lower-function would be called with these arguments:"
   @fact-output => #"\[5\]"))




;; You also get a helpful failure for an unused prerequisite:

(capturing-failure-output
 (fact
   (top-function 5) => 5555
   (provided
     (lower-function 3) => 5000    ; unused
     (lower-function 4) =>  500    ; unused
     (lower-function 5) =>   50    ; omit this one.
     (lower-function 6) =>    5))
 ;; So...
 (fact
   @fact-output => #"These calls were not made the right number of times:"
   @fact-output => #"\(lower-function 3\) \[expected at least once, actually never called\]"
   @fact-output => #"\(lower-function 4\) \[expected at least once, actually never called\]"
   ;; You also get a message about the failure:
   (strip-ansi-coloring @fact-output) => #"Expected:\n5555"
   (strip-ansi-coloring @fact-output) => #"Actual:\n55"))


                                ;;; Call counts

;; By default, prerequisites can be called one or more times. The
;; :times modifier lets you change that. Here's how you can insist
;; a prerequisite be called twice:

(capturing-failure-output
 (fact
   (top-function 5) => 55
   (provided
     (lower-function 5) => 50 :times 2
     (lower-function 6) =>  5))
 ;; So...
 (fact
   @fact-output => #"\(lower-function 5\) \[expected :times 2, actually called one time\]"))

; You can also give a range of allowed values. Here's how you'd ask
; for a function to be called one or two times:

(fact
  (top-function 5) => 55
  (provided
    (lower-function 5) => 50 :times [1 2]
    (lower-function 6) =>  5))

;; You can also use a lazy sequence:

(silent-fact
 (top-function 5) => 55
 (provided
   (lower-function 5) => 50 :times (range 3 33)
   (lower-function 6) =>  5))

(note-that fact-fails
           (prerequisite-was-called-the-wrong-number-of-times #"lower-function 5" 1 :time))


;; Here is the idiom for "this call is optional" (zero or more times)

(fact
  (top-function 5) => 55
  (provided
    (lower-function 0) => 88 :times (range)
    (lower-function 5) => 50
    (lower-function 6) =>  5))

;; The idiom for saying a function is not called is annoying:

(fact "how to say a function is not called"
  (+ 1 1) => 2
  (provided
    (top-function anything) => irrelevant :times 0))


;;;                         Prerequisites use a variant of extended-equality


(fact "You can use checkers"
  (+ 1 (lower-function 5.01)) => 8
  (provided
    (lower-function (roughly 5 0.1)) => 7))

(fact "You can use regular expressions"
  (str "this: " (lower-function "hello,           world")) => "this: worked"
  (provided
    (lower-function #"hello,\s+world") => "worked"))

;;; However, plain (non-checker) functions are matched literally. In the following,
;;; the `provided` means that the the specific function `even?` is to be passed to
;;; `hilbertian`.

(unfinished hilbertian)

(defn function-under-test [n]
  (hilbertian (if (pos? n) even? odd?)))

(fact
  (function-under-test 3) => ..hilbertian-result..
  (provided
    (hilbertian even?) => ..hilbertian-result..))

;;; If you want a plain function to be interpreted as a checker, do this:

(fact "Saying "
  (+ 1 (lower-function 2)) => 8
  (provided
    (lower-function (as-checker even?)) => 7))


;;; Prerequisites can ignore uninteresting trailing arguments by using the
;;; & marker followed by `anything`.
(unfinished letter)
(defn find-letter [] (letter "x" "y" "z"))

(fact "You can only care about the first argument"
  (fact "an example using a literal"
    (find-letter) => ..letter-result..
    (provided
      (letter "x" & anything) => ..letter-result..))
  (fact "an example using a checker argument"
    (find-letter) => ..letter-result..
    (provided
      (letter (as-checker string?) & anything) => ..letter-result..))
  (silent-fact (find-letter) => ..letter-result..
    (provided
      (letter "y" & anything) => ..letter-result..))
  (note-that some-prerequisite-was-called-with-unexpected-arguments))

(fact "Here is an idiom for saying that you care only that a sub-function has been called"
  ;; A bit wordy
  (find-letter) => anything
  (provided
    (letter & anything) => anything))

(let [desired-letter "x"]
  (fact "lexical bindings are obeyed"
    (find-letter) => ..letter-result..
    (provided
      (letter desired-letter & anything) => ..letter-result..)))

(fact "You can even apply a checker to the &rest argument"
  (find-letter) => ..letter-result..
  (provided
    (letter & (just "this" "doesn't" "match")) => ..bogus-result.. :times 0
    (letter & (contains ["x" "y"])) => ..letter-result..))






                                ;;; Nested functions in prerequisites

(unfinished first-est second-est)

;; This function's correctness depends on how `first-est` is called and on how
;; `second-est` is composed with `first-est`.
(defn function-under-test [n]
 (-> (first-est 1 n) second-est inc))

;; That could be expressed like this:

(fact
  (function-under-test 5) => 101
  (provided
    (first-est 1 5) => ..some-result..
    (second-est ..some-result..) => 100))

;; But it could be more tersely expressed like this:

(fact
  (function-under-test 5) => 101
  (provided
    (second-est (first-est 1 5)) => 100))

(defn my-inc [a b] (- (+ a (inc b)) b))
(defn strange-adder [a b]
  (+ (my-inc a) b))

(silent-fact
  (strange-adder 1 2) => 300
  (provided (my-inc 1) => 298))

(note-that fact-fails)
