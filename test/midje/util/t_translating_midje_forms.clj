(ns midje.util.t-translating-midje-forms
  (:use [midje.util.translating-midje-forms] :reload-all)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use [midje.util.thread-safe-var-nesting]
	[midje.util.wrapping :only [?form]])
  (:use [midje.util.building-midje-forms])
  (:use clojure.contrib.pprint))

(fact "human-friendly background forms can be expanded appropriately"
  (expand-background-shorthand-forms []) => []
  (expand-background-shorthand-forms '[(f 1) => 2]) =>
                                      '[(midje.semi-sweet/fake (f 1) => 2 :type :background)]
  (expand-background-shorthand-forms '[   (f 1) => 2 :foo 'bar (f 2) => 33 ]) => 
                              '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar :type :background)
                              (midje.semi-sweet/fake (f 2) => 33 :type :background) ])


;; Note: the explicit stack discipline is because "midjcoexpansion" happens before
;; macroexpansion (mostly) and so a with-pushed-namespace-values would not perform the
;; push at the right moment.
(push-into-namespace :midje/wrappers '[ (let [x 1] (?form)) ] )

(defmacro simulated-wrapper [form]
  (let [f (midjcoexpand form)]
;    (println f)
    f))

(fact "expect forms are wrapped"
  (simulated-wrapper (expect 1 => 1))
  (simulated-wrapper (expect x => 1)))

(fact "not all forms are wrapped"
  (let [x "not shadowed"]
    (expect (simulated-wrapper (str "is " x)) => "is not shadowed")))

(push-into-namespace :midje/wrappers '[ (let [x 33 y 12] ?form)
					(let [y 10] ?form) ])
(simulated-wrapper (expect (+ x y) => 43))
(pop-from-namespace :midje/wrappers)

(fact "nested expect forms are wrapped"
  (simulated-wrapper (do (expect x => 1))))

(fact "facts are expanded"
  (simulated-wrapper (fact x => 1)))

(pop-from-namespace :midje/wrappers)
