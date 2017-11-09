(ns midje.emission.plugins.t-default
  (:require [midje
             [sweet :refer :all]
             [util :refer :all]
             [test-util :refer :all]]
            [midje.emission.plugins.default :as plugin]
            [midje.emission.state :as state]
            [clojure.string :as str]))

(defn innocuously [key & args]
  (captured-output (apply (key plugin/emission-map) args)))

(fact "passes produce no output"
  (innocuously :pass) => "")

(fact "starting top-level-fact produces no output"
  (innocuously :starting-to-check-top-level-fact ..ignored..) => "")

(fact "report fact being entered"
  (let [name+desc-fact (with-meta (fn[])
                         {:midje/name "named" :midje/description "desc"})
        desc-fact (with-meta (fn[]) {:midje/description "desc"})
        unnamed (with-meta (fn[]) {:midje/file "file" :midje/line 3})]


    (fact "prints names in preference to descriptions"
      (innocuously :starting-to-check-fact name+desc-fact) => #"Checking named"
      (innocuously :starting-to-check-fact desc-fact) => #"Checking desc"
      (innocuously :starting-to-check-fact unnamed) => #"Checking fact at \(file:3\)")))

(fact "reports only when namespace changes"
   (plugin/set-last-namespace-shown! 'nothing)
   (innocuously :possible-new-namespace 'nothing) => ""
   (innocuously :possible-new-namespace 'something) => #"something"
   (innocuously :possible-new-namespace 'something) => "")

(fact "produces a summary from state information"
  (let [minimal-midje {:midje-passes 0
                       :midje-failures 0}
        minimal-ct {:test 0}]
    (innocuously :finishing-fact-stream minimal-midje minimal-ct)
    => #"No facts were checked. Is that what you wanted\?"

    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-passes 1) minimal-ct)
    => #"All checks \(1\) succeeded."

    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-failures 1 :midje-passes 0) minimal-ct)
    => #"FAILURE.*1 check failed.\s*$"

    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-failures 3 :midje-passes 0) minimal-ct)
    => #"FAILURE.*3 checks failed.\s*$"

    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-failures 3 :midje-passes 1) minimal-ct)
    => #"FAILURE.*3 checks failed.*But 1 succeeded"

    (innocuously :finishing-fact-stream (assoc minimal-midje :midje-failures 3 :midje-passes 2) minimal-ct)
    => #"FAILURE.*3 checks failed.*But 2 succeeded"


    (str/split-lines (innocuously :finishing-fact-stream
                                  (assoc minimal-midje :midje-failures 0 :midje-passes 2)
                                  {:test 3
                                   :fail 0
                                   :error 1
                                   :lines ["line 1"
                                           "line 2"
                                           "successes"
                                           "failures"]}))
    => (contains #"Midje summary"
                 #"All checks \(2\) succeeded."
                 #""
                 #"Output from clojure.test"
                 #"line 1"
                 #"line 2"
                 #"successes"
                 #"failures")))
