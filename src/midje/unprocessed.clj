(ns midje.unprocessed
  (:use clojure.test
	[midje.unprocessed unprocessed-internals background]
        midje.util.report
        clojure.contrib.error-kit
        [clojure.contrib.ns-utils :only [immigrate]]))
(immigrate 'midje.util.checkers)




(defn expect* [call-map local-expectations]
  "The core function in unprocessed Midje. Takes a map describing a call and a 
   list of maps, each of which describes a secondary call the first call is supposed to 
   make. See the documentation at http://github.com/marick/Midje."
  (let [expectations (adding-background local-expectations)]
    (with-altered-roots (binding-map expectations)
      (let [code-under-test-result (capturing-exception
				    (eagerly
				     ((call-map :function-under-test))))]
	(check-call-counts expectations)
	(check-result code-under-test-result call-map)))))
      

