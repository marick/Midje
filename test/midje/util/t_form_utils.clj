(ns midje.util.t-form-utils
  (:use [midje.util.form-utils] :reload-all)
  (:use [midje.sweet])
  (:use [midje.test-util]))


(facts "a form's reader-assigned line-number can be extracted"
  (reader-line-number (with-meta '(fact (this that)) {:line 23})) => 23
  "or, failing that: try top-level subforms"
  (reader-line-number `(fact
			 (+ 1 2)
			 ~(with-meta '(this that) {:line 23})
			 ~(with-meta '(this that) {:line 22223}))) => 23
  "or a default value"
  (reader-line-number (with-meta '(fact "text") {})) => "0 (no line info)")

(facts "sometimes it's useful to flatten and remove nils"
  (flatten-and-remove-nils '()) => []
  (flatten-and-remove-nils '(nil "foo" ("bar" nil "baz"))) => ["foo" "bar" "baz"])
