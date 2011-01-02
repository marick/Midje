(ns midje.t-fakes
  (:use [midje fakes sweet test-util]))

(declare f g)
(fact "unique variables can be found in fakes"
  (let [fakes [ (fake (f 1) => 2)
		(fake (f 2) => 4)
		(fake (g) => 3)] ]
    (unique-function-vars fakes) => (contains [#'f #'g] :in-any-order)))

(facts "matching calls depend on both function name and arguments"
  (let [fake {:function 'f, :arg-matchers [ odd? ] }]
    (find-matching-call 'g [3] [fake]) => falsey
    (find-matching-call 'f [3 3] [fake]) => falsey
    (find-matching-call 'f [4] [fake]) => falsey
    (find-matching-call 'f [3] [fake]) => fake))

(fact "fakes keep track of their call counts"
  (let [fakes [(fake (f 1) => 3)
	       (fake (g 1) => 4)
	       (fake (f 2) => 5)]
	counts #(map fake-count fakes)]
    (call-faker #'f [1] fakes)    (counts) => [1 0 0]
    (call-faker #'f [1] fakes)    (counts) => [2 0 0]
    (call-faker #'f [2] fakes)    (counts) => [2 0 1]
    (call-faker #'g [1] fakes)    (counts) => [2 1 1]))

(fact "Unintuitively, earlier binding maps override later"
  (let [fakes [(fake (f 1) => 3 :type :background)
	       (fake (f 1) => 4 :type :background)]
	result-map (binding-map fakes)]

    (call-faker (var f) [1] fakes)
    (map fake-count fakes) => [1 0]))
