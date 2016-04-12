(ns as-documentation.production-mode
  (:require [midje.sweet :refer :all]))

;;; People sometimes intermix facts with production code. When you actually deploy the production code,
;;; you may not want the Midje facts checked at load time. "Production mode" prevents that.

;;; (Note: checking facts at load time may do little harm: why not run
;;; the tests when you deploy? Loaded facts don't take up too much space.)

;;; Either midje.sweet/*include-midje-checks* or clojure.test/*load-tests* can be used to
;;; turn tests off.

(alter-var-root #'midje.sweet/include-midje-checks (constantly false))

(fact
  (throw (new Error "Should not be called.")))

(facts
  (throw (new Error "Should not be called.")))

(fact-group
 (throw (new Error "Should not be called.")))

(with-state-changes []
  (throw (new Error "Should not be called.")))

(alter-var-root #'midje.sweet/include-midje-checks (constantly true))
(alter-var-root #'clojure.test/*load-tests* (constantly false))

(fact
  (throw (new Error "Should not be called.")))

(facts
  (throw (new Error "Should not be called.")))

(fact-group
 (throw (new Error "Should not be called.")))

(with-state-changes []
  (throw (new Error "Should not be called.")))

(alter-var-root #'clojure.test/*load-tests* (constantly true))
