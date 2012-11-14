(ns midje.util.t-ecosystem
  (:use [midje.sweet]
        [clojure.pprint]
        [midje.test-util]
        [midje.util.ecosystem]))

                                        ;;; Facts on disk

(facts "locating fact namespaces"
  (fact "defaults to test directory"
    (let [default-namespaces (fact-namespaces)]
      default-namespaces => (contains 'midje.ideas.t-rerunning-facts)
      default-namespaces =not=> (contains 'midje.ideas.rerunning-facts)))

  (fact "can be given explicit directories" 
    (let [chosen-namespaces (fact-namespaces "src")]
      chosen-namespaces =not=> (contains 'midje.ideas.t-rerunning-facts)
      chosen-namespaces => (contains 'midje.ideas.rerunning-facts))
    (let [chosen-namespaces (fact-namespaces "src" "test")]
      chosen-namespaces => (contains #{'midje.ideas.rerunning-facts
                                       'midje.sweet}
                                     :gaps-ok)
      chosen-namespaces => (contains 'behaviors.t-isolated-metaconstants)))

  (fact "can filter by prefix"
    (let [default-prefix (fact-namespaces :prefix "midje.checkers")]
      default-prefix => (contains 'midje.checkers.t-chatty)
      default-prefix =not=> (contains 'midje.ideas.t-rerunning-facts)
      default-prefix =not=> (contains 'midje.ideas.rerunning-facts))

    (let [chosen-prefix (fact-namespaces "src" :prefix "midje.ideas")]
      chosen-prefix => (contains 'midje.ideas.rerunning-facts)
      chosen-prefix =not=> (contains 'midje.checkers.chatty)
      chosen-prefix =not=> (contains 'midje.ideas.t-rerunning-facts))

    ;; truly a prefix
    (fact-namespaces "src" :prefix "ideas") => empty?))

