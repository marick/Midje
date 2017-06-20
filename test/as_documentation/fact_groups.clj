(ns as-documentation.fact-groups
  (:require [midje.config :as config]
            [midje.repl :refer :all]
            [midje.test-util :refer :all]))

;;; Fact groups are used to supply metadata to the enclosed top-level facts.
;;; For example, here are two facts that would cause failures if checked.
;;; They're not checked because we'll only check facts that aren't slow.

(config/with-augmented-config {:check-after-creation false}
  (fact-group :slow
    (fact "group 1a" 1 => :will-fail-when-run)
    (fact "group 1b" 1 => :will-fail-when-run)))

(check-facts *ns* :print-no-summary :filter (complement :slow)) ; No failures

;;; Enclosed facts can override fact-group metadata.

(def has-fact-2a-run? (atom false))
(config/with-augmented-config {:check-after-creation false}
  (fact-group :slow
    (fact "group 2a" {:slow false} (reset! has-fact-2a-run? true))
    (fact "group 2b" 1 => :will-fail-when-run)))

(check-facts *ns* :print-no-summary :filter (complement :slow)) ; No failures
(fact @has-fact-2a-run? => true)

;;; Note that metadata only affects *top level* facts. So you can't put
;;; metadata on a top-level fact and expect it to be overridden by an
;;; enclosed fact.

(config/with-augmented-config {:check-after-creation false}
  (fact :slow
    ;; The metadata below does nothing.
    (fact {:slow false} "fact-third" => :will-fail-when-run)))

(check-facts *ns* :print-no-summary :filter (complement :slow))
