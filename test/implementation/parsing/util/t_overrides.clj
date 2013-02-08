(ns implementation.parsing.util.t_overrides
  (:use midje.sweet
        midje.test-util
        midje.parsing.util.overrides))

(fact "when at end of required part of arrow form, can ask for overrides"
    "empty rest of form"
    (arrow-sequence-overrides '()) => '()

    "new arrow form"
    (arrow-sequence-overrides '((g 1) => 1)) => '()

    "typical example of arrow-sequence-overrides"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"))
    => '(:expected-result 3 :position "foo.clj:33")

    "Does not scoop up following forms"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33" (f 1)))
    => '(:expected-result 3 :position "foo.clj:33")

    "... even if those following forms have their own overrides"
    (arrow-sequence-overrides '( :expected-result 3 :position "foo.clj:33"
                                   (f 1) => 1 :expected-result 2))
    => '(:expected-result 3 :position "foo.clj:33"))
