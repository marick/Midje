(ns as-documentation.configuration
  (:require [midje.sweet :refer :all]
            [midje.test-util :refer :all]))


;;; To change configuration settings for your whole project, you use
;;; configuration files. The default name for such a file is
;;; `.midje.clj` in the root of the project. You can also put a
;;; `.midje.clj` file in your home directory. It applies to all
;;; projects. You change a setting with code like this:
;;;
;;;   (change-defaults :visible-deprecation false
;;;                    :visible-future false)
;;;
;;; Different configuration files can be specified with the `:config`
;;; option to `lein midje`. See `lein help midje` for more.

;;; You may occasionally want to change configuration
;;; settings within a single file. `change-defaults` does
;;; NOT work for that purpose. Instead, you use
;;; `with-augmented-config` as shown below.


;; The config namespace is not automatically available.
(require '[midje.config :as config])

(fact "We're going to change this value"
  (config/choice :print-level) =not=> :print-nothing)

(fact "with-augmented-config changes a config setting within a scope"
  (config/with-augmented-config {:print-level :print-nothing}
    (config/choice :print-level) => :print-nothing))

(fact "changes are restricted to that scope"
  (config/choice :print-level) =not=> :print-nothing)


;; Changing the print level is a common enough operation
;; (for at least Midje's own tests) that it has its own
;; shorthand:


(fact "`at-print-level` changes the print level within a scope"
  (config/at-print-level :print-nothing
    (config/choice :print-level) => :print-nothing))

(fact "changes are restricted to that scope"
  (config/choice :print-level) =not=> :print-nothing)
