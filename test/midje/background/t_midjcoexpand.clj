(ns midje.background.t-midjcoexpand
  (:use midje.sweet)
  (:use [midje.util.thread-safe-var-nesting])
  (:use midje.background.midjcoexpand))

(push-into-namespace :midje/wrappers '[ (let [x 1] ?form) ] )

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
