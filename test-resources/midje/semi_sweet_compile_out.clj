
(require 'clojure.test)
;; This file is used to check whether checks can be compiled out.
(expect (throw (Exception. "Should never be called"))  => true)
(clojure.test/deftest tests-can-be-compiled-out
    (expect false => true))
