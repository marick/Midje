(ns midje.ideas.reporting.t-levels
  (:use midje.ideas.reporting.level-defs
        midje.ideas.reporting.levels
        [midje sweet util test-util]
        [midje.clojure-test-facade :only [counters]])
  (:require [midje.config :as config]))
(expose-testables midje.ideas.reporting.levels)

(facts "about levels"
  (-> -2 levels-to-names names-to-levels) => -2
  (-> -1 levels-to-names names-to-levels) => -1
  (-> 0 levels-to-names names-to-levels) => 0
  (-> 1 levels-to-names names-to-levels) => 1
  (-> 2 levels-to-names names-to-levels) => 2

  (-> :print-nothing names-to-levels levels-to-names) => :print-nothing
  (-> :print-no-summary names-to-levels levels-to-names) => :print-no-summary
  (-> :print-normally names-to-levels levels-to-names) => :print-normally
  (-> :print-namespaces names-to-levels levels-to-names) => :print-namespaces
  (-> :print-facts names-to-levels levels-to-names) => :print-facts

  (validate-level! :print-namespaces) => anything
  (validate-level! :print-namespace ) => (throws #":print-namespace.*valid")
  (validate-level! 500) => (throws #"500.*valid")

  (normalize :print-nothing) => (names-to-levels :print-nothing)
  (normalize :a-mistake) => (throws ":a-mistake is not a valid :print-level.")
  (normalize 800) => (throws "800 is not a valid :print-level."))
  
(facts "separating levels out of argument lists"
  (separate-print-levels [])
  => [(names-to-levels :print-normally) nil]
  (separate-print-levels [:print-nothing])
  => [(names-to-levels :print-nothing) nil]
  (separate-print-levels [:all :print-nothing])
  => [(names-to-levels :print-nothing) [:all]]
  (separate-print-levels [:print-nothing :all])
  => [(names-to-levels :print-nothing) [:all]]
  (separate-print-levels ['a (names-to-levels :print-facts) 'b])
  => [(names-to-levels :print-facts) '[a b]])
  
(fact "can override print levels temporarily"
  (config/choice :print-level) => :print-normally
  (obeying-print-levels [args [:foo :bar :print-facts]]
     args => [:foo :bar]
     (config/choice :print-level) => (names-to-levels :print-facts))
  (let [args '[a b 2]]
    (obeying-print-levels [args args]
      args => '[a b]
      (config/choice :print-level) => 2)))
                        

(fact "report fact being checked"
  (let [name+desc-fact (with-meta (fn[])
                         {:midje/name "named" :midje/description "desc"})
        desc-fact (with-meta (fn[]) {:midje/description "desc"})
        unnamed (with-meta (fn[]) {:midje/file "file" :midje/line 3})]
    (fact "prints nothing unless at :print-facts level"
      (config/with-temporary-config
        {:print-level (dec (names-to-levels :print-facts))}
        (with-out-str (report-checking-fact name+desc-fact)) => ""))

    (fact "prints names in preference to descriptions"
      (config/with-temporary-config {:print-level :print-facts}
        (with-out-str (report-checking-fact name+desc-fact)) => #"Checking named"
        (with-out-str (report-checking-fact desc-fact)) => #"Checking desc"
        (with-out-str (report-checking-fact unnamed))
        => #"Checking fact at \(file:3\)"))))

(fact "report-summary"
  (config/with-temporary-config {:print-level :print-no-summary}
    (with-out-str (report-summary)) => "")
  
  (config/with-temporary-config
    {:print-level (inc (names-to-levels :print-no-summary))}
    (with-out-str (report-summary)) => #"All claims.*have been confirmed"
    (provided
      (counters) => {:fail 0 :pass 1})))
  
(fact "report on a namespace"
  (fact "Not normally printed"
    (with-out-str (report-changed-namespace 'ignored)) => ""
    (config/with-temporary-config
      {:print-level (dec (names-to-levels :print-namespaces))}
      (with-out-str (report-changed-namespace 'ignored)) => ""))

  (fact "reports only when namespace changes"
    (config/with-temporary-config {:print-level :print-namespaces}
      (reset! last-namespace-shown "nothing")
      (with-out-str (report-changed-namespace "nothing")) => ""
      (with-out-str (report-changed-namespace "something")) => #"something")))


