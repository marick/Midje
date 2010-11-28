(ns midje.midje-forms.t-translating
  (:use [midje.midje-forms.translating] :reload-all)
  (:use [midje.sweet])
  (:use [midje.test-util])
  (:use [midje.util thread-safe-var-nesting unify]
	[midje.util.wrapping :only [?form]])
  (:use [midje.midje-forms.building])
  (:use clojure.contrib.pprint))
(testable-privates midje.midje-forms.translating
		   canonicalize-raw-wrappers make-final replace-with-magic-form)

(fact "human-friendly background forms can be canonicalized appropriately"
  "fakes"
  (canonicalize-raw-wrappers []) => []
  (canonicalize-raw-wrappers '[(f 1) => 2]) =>
                                 '[(midje.semi-sweet/fake (f 1) => 2 :type :background)]
  (canonicalize-raw-wrappers '[   (f 1) => 2 :foo 'bar (f 2) => 33 ]) => 
                              '[(midje.semi-sweet/fake (f 1) => 2 :foo 'bar :type :background)
				(midje.semi-sweet/fake (f 2) => 33 :type :background) ]

  "other types are left alone"
  (canonicalize-raw-wrappers
   '[ (before :checking (swap! test-atom (constantly 0))) ]) =>
   '[ (before :checking (swap! test-atom (constantly 0))) ]

 "mixtures"
 (canonicalize-raw-wrappers
   '[ (f 1) => 2 (before :checking (swap! test-atom (constantly 0))) (f 2) => 3 ]) =>
   '[ (midje.semi-sweet/fake (f 1) => 2 :type :background)
      (before :checking (swap! test-atom (constantly 0)))
      (midje.semi-sweet/fake (f 2) => 3 :type :background) ]
 
 "error cases"
 (canonicalize-raw-wrappers '[ (after anything) ]) => (throws Error)
 )

;; You can't refer to the magical symbol that's used in wrapper substitution because
;; it will then be substituted. So this turns it into a string.
(defn guard-special-form [bindings]
  (assoc (dissoc bindings '?danger) '?danger (str (bindings '?danger))))

(fact "canonicalized setup/teardown wrappers can be put into final form"
  (let [bindings (unify '(try (do-something) ?danger (finally nil))
			(make-final '(before :checking (do-something))))]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" })
  
  (let [bindings (unify '(try (do-something) ?danger (finally (finish)))
			(make-final '(before :checking (do-something) :after (finish))))]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" })
 
  (let [bindings (unify '(try ?danger (finally (do-something)))
			(make-final '(after :checking (do-something))))]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" })

  (let [bindings (unify '(let [x 1] ?danger)
			(make-final '(around :checking (let [x 1] ?form))))]
    (guard-special-form bindings) => { '?danger "midje.midje-forms.t-translating/?form" })
)
;; Note: the explicit stack discipline is because "midjcoexpansion" happens before
;; macroexpansion (mostly) and so a with-pushed-namespace-values would not perform the
;; push at the right moment.
(do 
  (push-into-namespace :midje/wrappers '[ (let [x 1] (?form)) ] )

  (defmacro simulated-wrapper [form]
    (let [f (midjcoexpand form)]
      ; (println f)
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
)
