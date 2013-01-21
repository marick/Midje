(ns midje.data.t-core-maps
  (:use midje.sweet
        midje.test-util
        midje.data.core-maps))

(facts "about arrows"
  (let [result (map check-for-arrow
                    '(=> midje.data.core-maps/=> midje.sweet/=>
                      =not=> midje.data.core-maps/=not=> midje.sweet/=not=>
                      =expands-to=> midje.semi-sweet/=expands-to=> midje.sweet/=expands-to=>))]
    (fact result => [:check-match :check-match :check-match
                     :check-negated-match :check-negated-match :check-negated-match
                     :check-match :check-match :check-match])))


(fact "make-example-map creates a map"
  (fact "base case"
    (let [result (make-example-map (+ 1 10) => (+ 9 990) [])]
      ((:function-under-test result)) => 11
      (:desired-check :check-match)
      (:expected-result result) => 999
      (:expected-result-form result) => '(+ 9 990)))
  
  (fact "with additional info for midje tool creators"
    (make-example-map (+ 1 1) =test=> 2 []) => (contains {:call-form '(+ 1 1) 
                                                          :arrow '=test=>
                                                          :expected-result 2}))
  
  (fact "containing fact descriptions"
    (make-example-map 1 =test=> 1 []) => (contains {:description ["make-example-map creates a map"
                                                                  "containing fact descriptions"]}))

  (fact "and additional information as given"
    (make-example-map 1 =test=> 1 [:other :stuff]) => (contains {:other :stuff})))


