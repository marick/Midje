(ns ^{:doc "An emission map whose functions can be mocked."}
  midje.emission.plugins.test-support)

(def recorder (atom :uninitialized))
(defn recorded [] @recorder)
(defn reset-recorder! []
  (reset! recorder []))

(defn record [cmd]
  (fn [& args]
    (swap! recorder #(conj % (cons cmd args)))))

(defn make-map [& keys]
  (zipmap keys
          (map record keys)))

(def emission-map (make-map :pass
                            :fail
                            :starting-to-check-top-level-fact
                            :finishing-top-level-fact
                            :starting-to-check-fact
                            :finishing-fact
                            :possible-new-namespace
                            :fact-stream-summary
                            :future-fact
                            :forget-everything))
