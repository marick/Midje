(ns ^{:doc "An emission map whose functions can be mocked."}
  midje.emission.plugins.test-support)

(def recorder (atom :uninitialized))
(defn recorded [] @recorder)
(defn reset-recorder! []
  (reset! recorder []))

(defn record [cmd]
  (fn [& args]
    (swap! recorder #(conj % (cons cmd args)))))

(def emission-map {:pass (record :pass)
                   :fail (record :fail)
                   :starting-to-check-fact (record :starting-to-check-fact)
                   :forget-everything (record :forget-everything)
                   })

