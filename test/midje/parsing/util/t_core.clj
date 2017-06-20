(ns midje.parsing.util.t-core
  (:require [midje.parsing.util.core :refer :all]
            [midje.sweet :refer :all]
            [midje.parsing.2-to-lexical-maps.expects :refer [expect]]
            [midje.parsing.2-to-lexical-maps.fakes :refer [fake]]
            [midje.parsing.2-to-lexical-maps.data-fakes :refer [data-fake]]
            [midje.test-util :refer :all]
            [clojure.zip :as zip]))

(facts "a form's reader-assigned line-number can be extracted"
  (reader-line-number (with-meta '(fact (this that)) {:line 23})) => 23
  "or, failing that: try top-level subforms"
  (reader-line-number `(fact
                         (+ 1 2)
                         ~(with-meta '(this that) {:line 23})
                         ~(with-meta '(this that) {:line 22223}))) => 23
  "or a default value"
  (reader-line-number (with-meta '(fact "text") {})) => "0 (no line info)")


(fact "can unquote a form"
  (dequote '1) => 1
  (dequote 1) => 1
  (dequote '(some form)) => '(some form))


